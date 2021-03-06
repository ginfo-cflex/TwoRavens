"""
Used to assist with TA2 calls, specifically:

(1) FitSolution
(2) GetFitSolutionResults (contains fitted_solution_id)
"""
import logging
from django.conf import settings

from tworaven_apps.ta2_interfaces.websocket_message import WebsocketMessage
from tworaven_apps.raven_auth.models import User
from tworaven_apps.utils.basic_err_check import BasicErrCheck
from tworaven_apps.utils.basic_response import (ok_resp, err_resp)
from tworaven_apps.utils.json_helper import \
    (json_loads, json_dumps, get_dict_value)
from tworaven_apps.utils.proto_util import message_to_json
from tworaven_apps.ta2_interfaces.ta2_connection import TA2Connection
from tworaven_apps.ta2_interfaces.stored_data_util import StoredRequestUtil
from tworaven_apps.ta2_interfaces.req_search_solutions import fit_solution
from tworaven_apps.ta2_interfaces.ta2_produce_solution_helper import ProduceSolutionHelper
from tworaven_apps.ta2_interfaces.models import \
        (StoredRequest, StoredResponse)
from tworaven_apps.ta2_interfaces import static_vals as ta2_static
from tworaven_apps.behavioral_logs.log_entry_maker import LogEntryMaker
from tworaven_apps.behavioral_logs import static_vals as bl_static

import core_pb2
import grpc
from google.protobuf.json_format import \
    (Parse, ParseError)
from tworavensproject.celery import celery_app

LOGGER = logging.getLogger(__name__)

class FitSolutionHelper(BasicErrCheck):
    """Helper class to run TA2 call sequence"""

    def __init__(self, pipeline_id, websocket_id, user_id, fit_params, **kwargs):
        """initial params"""
        self.pipeline_id = pipeline_id
        self.websocket_id = websocket_id
        self.user_id = user_id
        self.user_object = None

        self.fit_params = fit_params

        self.search_id = kwargs.get('search_id', None)
        self.produce_params = kwargs.get('produce_params')

        self.produce_dataset_name = kwargs.get('produce_dataset_name')

        self.session_key = kwargs.get('session_key', '')

        self.get_user()
        self.check_fit_params()


    def get_user(self):
        """Fetch the user"""
        if self.has_error():
            return

        try:
            self.user_object = User.objects.get(pk=self.user_id)
        except User.DoesNotExist:
            self.add_err_msg('No user found for id: %s' % self.user_id)


    def check_fit_params(self):
        """Check that "fit_params" has all of the required sections
          Except for 'solutionId', params set at:
            file: app.js
            function: getFitSolutionDefaultParameters
        """
        if self.has_error():
            return False

        if not isinstance(self.fit_params, dict):
            self.add_err_msg('fit params must be a python dict')
            return False

        # Iterate through the expectd keys
        #
        expected_keys = [ta2_static.KEY_SOLUTION_ID, 'inputs', 'exposeOutputs',
                         'exposeValueTypes', 'users']

        for key in expected_keys:
            if not key in self.fit_params:
                user_msg = ('fit_params is missing key: %s') % \
                            (self.pipeline_id, key)
                self.send_websocket_err_msg(ta2_static.FIT_SOLUTION, user_msg)
                return False

        return True

    @staticmethod
    @celery_app.task(ignore_result=True)
    def make_fit_solutions_call(pipeline_id, websocket_id, user_id, fit_params, **kwargs):
        print('make_fit_solutions_call 1')
        assert pipeline_id, "pipeline_id must be set"
        assert websocket_id, "websocket_id must be set"
        assert user_id, "user_id must be set"
        assert fit_params, "fit_params must be set"
        fit_helper = FitSolutionHelper(pipeline_id, websocket_id,
                                       user_id, fit_params, **kwargs)

        if fit_helper.has_error():
            user_msg = ('FitSolution failure for pipeline (%s): %s') % \
                        (pipeline_id, fit_helper.get_error_message())

            ws_msg = WebsocketMessage.get_fail_message(\
                        ta2_static.FIT_SOLUTION, user_msg)

            ws_msg.send_message(websocket_id)
            LOGGER.error(user_msg)
            return


        fit_helper.run_process()



    def run_process(self):
        """(1) Run FitSolution"""
        if self.has_error():
            return
        # ----------------------------------
        # Create the input
        # ----------------------------------
        LOGGER.info('FitSolutionHelper.run_process 2')
        json_str_info = json_dumps(self.fit_params)
        if not json_str_info.success:
            self.add_err_msg(json_str_info.err_msg)
            return

        json_str_input = json_str_info.result_obj

        stored_request = StoredRequest(\
                        user=self.user_object,
                        search_id=self.search_id,
                        pipeline_id=self.pipeline_id,
                        workspace='(not specified)',
                        request_type=ta2_static.FIT_SOLUTION,
                        is_finished=False,
                        request=self.fit_params)
        stored_request.save()

        # --------------------------------
        # (2a) Behavioral logging
        # --------------------------------
        log_data = dict(session_key=self.session_key,
                        feature_id=ta2_static.FIT_SOLUTION,
                        activity_l1=bl_static.L1_MODEL_SELECTION,
                        activity_l2=bl_static.L2_MODEL_EXPLANATION,
                        other=self.fit_params)

        LogEntryMaker.create_ta2ta3_entry(self.user_object, log_data)

        # ----------------------------------
        # Run FitSolution
        # ----------------------------------
        fit_info = fit_solution(json_str_input)
        if not fit_info.success:
            self.send_websocket_err_msg(ta2_static.FIT_SOLUTION,
                                        fit_info.err_msg)
            StoredResponse.add_err_response(\
                    stored_request,
                    fit_info.err_msg)
            return

        # ----------------------------------
        # Parse the FitSolutionResponse
        # ----------------------------------
        response_info = json_loads(fit_info.result_obj)
        if not response_info.success:
            StoredResponse.add_err_response(\
                    stored_request,
                    response_info.err_msg)
            self.send_websocket_err_msg(ta2_static.FIT_SOLUTION, response_info.err_msg)
            return

        result_json = response_info.result_obj


        # ----------------------------------
        # Get the requestId
        # ----------------------------------
        if not ta2_static.KEY_REQUEST_ID in result_json:
            user_msg = (' "%s" not found in response to JSON: %s') % \
                        (ta2_static.KEY_REQUEST_ID, result_json)
            #
            StoredResponse.add_err_response(stored_request, user_msg)
            #
            self.send_websocket_err_msg(ta2_static.FIT_SOLUTION, user_msg)
            return

        StoredResponse.add_success_response(stored_request,
                                            result_json)

        self.run_get_fit_solution_responses(result_json[ta2_static.KEY_REQUEST_ID])


    def send_websocket_err_msg(self, grpc_call, user_msg=''):
        """Send an error messsage over websockets"""
        assert grpc_call, 'grpc_call is required'

        user_msg = '%s error; pipeline %s: %s' % \
                   (grpc_call,
                    self.pipeline_id,
                    user_msg)

        # ----------------------------------
        # Send Websocket message
        # ----------------------------------
        ws_msg = WebsocketMessage.get_fail_message(grpc_call, user_msg)
        ws_msg.send_message(self.websocket_id)

        # ----------------------------------
        # Log it
        # ----------------------------------
        LOGGER.info('FitSolutionHelper: %s', user_msg)

        # ----------------------------------
        # Add error message to class
        # ----------------------------------
        self.add_err_msg(user_msg)


    def run_get_fit_solution_responses(self, request_id):
        """(2) Run GetFitSolutionResults"""
        if self.has_error():
            return

        if not request_id:
            self.send_websocket_err_msg(ta2_static.GET_FIT_SOLUTION_RESULTS,
                                        'request_id must be set')
            return

        # -----------------------------------
        # (1) make GRPC request object
        # -----------------------------------
        params_dict = {ta2_static.KEY_REQUEST_ID: request_id}
        params_info = json_dumps(params_dict)

        try:
            grpc_req = Parse(params_info.result_obj,
                             core_pb2.GetFitSolutionResultsRequest())
        except ParseError as err_obj:
            err_msg = ('Failed to convert JSON to gRPC: %s') % (err_obj)
            self.send_websocket_err_msg(ta2_static.GET_FIT_SOLUTION_RESULTS,
                                        err_msg)
            return

        # --------------------------------
        # (2) Save the request to the db
        # --------------------------------
        stored_request = StoredRequest(\
                        user=self.user_object,
                        request_type=ta2_static.GET_FIT_SOLUTION_RESULTS,
                        pipeline_id=self.pipeline_id,
                        search_id=self.search_id,
                        is_finished=False,
                        request=params_dict)
        stored_request.save()

        # --------------------------------
        # (2a) Behavioral logging
        # --------------------------------
        log_data = dict(session_key=self.session_key,
                        feature_id=ta2_static.GET_FIT_SOLUTION_RESULTS,
                        activity_l1=bl_static.L1_MODEL_SELECTION,
                        activity_l2=bl_static.L2_MODEL_EXPLANATION,
                        other=params_dict)

        LogEntryMaker.create_ta2ta3_entry(self.user_object, log_data)

        # --------------------------------
        # (3) Make the gRPC request
        # --------------------------------
        core_stub, err_msg = TA2Connection.get_grpc_stub()
        if err_msg:
            return err_resp(err_msg)

        msg_cnt = 0
        try:
            # -----------------------------------------
            # Iterate through the streaming responses
            # Note: The StoredResponse.id becomes the pipeline id
            # -----------------------------------------
            for reply in core_stub.GetFitSolutionResults(\
                    grpc_req, timeout=settings.TA2_GRPC_LONG_TIMEOUT):

                msg_cnt += 1

                stored_response = None  # to hold a StoredResponse object

                # -----------------------------------------------
                # Parse the response into JSON + store response
                # -----------------------------------------------
                msg_json_str = message_to_json(reply)
                msg_json_info = json_loads(msg_json_str)

                if not msg_json_info.success:
                    err_msg = ('Failed to convert JSON to gRPC: %s') % \
                               (err_obj,)

                    StoredResponse.add_stream_err_response(\
                                        stored_request, err_msg)

                    self.send_websocket_err_msg(\
                            ta2_static.GET_FIT_SOLUTION_RESULTS,
                            err_msg)
                    # Wait for next response....
                    continue

                result_json = msg_json_info.result_obj

                if ta2_static.KEY_FITTED_SOLUTION_ID not in result_json:
                    user_msg = '"%s" not found in response to JSON: %s' % \
                               (ta2_static.KEY_FITTED_SOLUTION_ID, result_json)

                    StoredResponse.add_stream_err_response(\
                                        stored_request, err_msg)

                    self.send_websocket_err_msg(\
                            ta2_static.GET_FIT_SOLUTION_RESULTS,
                            err_msg)
                    # Wait for next response....
                    continue

                fitted_solution_id = result_json[ta2_static.KEY_FITTED_SOLUTION_ID]

                # -----------------------------------------
                # Looks good, save the response
                # -----------------------------------------
                save_resp_info = StoredResponse.add_stream_success_response(\
                                        stored_request, result_json)

                if not save_resp_info.success:
                    # shouldn't happen...
                    StoredResponse.add_stream_err_response(\
                                    stored_request, save_resp_info.err_msg)
                    continue

                # ---------------------------------------------
                # Looks good!  Get the StoredResponse
                # - send responses back to WebSocket
                # ---------------------------------------------
                stored_response = save_resp_info.result_obj

                # ---------------------------------------------
                # If progress is complete,
                #  send response back to WebSocket
                # ---------------------------------------------
                progress_val = get_dict_value(\
                                result_json,
                                [ta2_static.KEY_PROGRESS, ta2_static.KEY_PROGRESS_STATE])

                if (not progress_val.success) or \
                   (progress_val.result_obj != ta2_static.KEY_PROGRESS_COMPLETED):
                    user_msg = 'GetFitSolutionResultsResponse is not yet complete'
                    LOGGER.info(user_msg)
                    # wait for next message...
                    continue


                ws_msg = WebsocketMessage.get_success_message(\
                            ta2_static.GET_FIT_SOLUTION_RESULTS,
                            'it worked',
                            msg_cnt=msg_cnt,
                            data=stored_response.as_dict())

                LOGGER.info('ws_msg: %s', ws_msg)
                #print('ws_msg', ws_msg.as_dict())

                ws_msg.send_message(self.websocket_id)

                # if GetFitSolutionResultsResponse is COMPLETE,
                #  then trigger ProduceSolution
                #
                if fitted_solution_id:

                    for produce_dataset_name in self.produce_params:
                        print('Make Produce Solution with:', produce_dataset_name)
                        self.check_fit_progress(self.produce_params[produce_dataset_name],
                                                fitted_solution_id,
                                                result_json,
                                                produce_dataset_name=produce_dataset_name)

        except grpc.RpcError as err_obj:
            stored_request.set_error_status(str(err_obj))
            return

        except Exception as err_obj:
            stored_request.set_error_status(str(err_obj))
            return


        StoredRequestUtil.set_finished_ok_status(stored_request.id)


    def check_fit_progress(self, params_for_produce,
                           fitted_solution_id, result_json, **kwargs):
        """if GetFitSolutionResultsResponse is COMPLETED,
           then trigger ProduceSolution"""
        assert isinstance(result_json, dict), 'result_json must be a dict'
        assert fitted_solution_id, 'fitted_solution_id must be set'

        produce_dataset_name = kwargs.get('produce_dataset_name')

        # --------------------------------------------
        # Check if the progress.state == 'COMPLETED'
        # --------------------------------------------
        progress_val = get_dict_value(\
                        result_json,
                        [ta2_static.KEY_PROGRESS,
                         ta2_static.KEY_PROGRESS_STATE])

        if (not progress_val.success) or \
           (progress_val.result_obj != ta2_static.KEY_PROGRESS_COMPLETED):
            user_msg = 'FitSolutionResultsResponse is not yet complete'
            LOGGER.info(user_msg)
            return

        # --------------------------------------------
        # Format ProduceSolution parameters
        # --------------------------------------------
        if not params_for_produce:
            user_msg = 'No params available for ProduceSolution'
            self.send_websocket_err_msg(\
                            ta2_static.GET_FIT_SOLUTION_RESULTS,
                            user_msg)
            LOGGER.error(user_msg)
            return

        print('produce 0')
        print(params_for_produce)

        prod_params = dict(params_for_produce)
        prod_params[ta2_static.KEY_FITTED_SOLUTION_ID] = fitted_solution_id

        print('produce 0.5')
        ProduceSolutionHelper.make_produce_solution_call.delay(\
                                    self.pipeline_id,
                                    self.websocket_id,
                                    self.user_id,
                                    prod_params,
                                    search_id=self.search_id,
                                    session_key=self.session_key,
                                    produce_dataset_name=produce_dataset_name)
