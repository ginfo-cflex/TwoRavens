"""
Code based on sample by Matthias Grabmair
    - https://gitlab.datadrivendiscovery.org/mgrabmair/ta3ta2-proxy
"""
import json
from collections import OrderedDict

from django.conf import settings
from google.protobuf.json_format import MessageToJson,\
    Parse, ParseError

from tworaven_apps.ta2_interfaces import core_pb2
from tworaven_apps.ta2_interfaces.ta2_connection import TA2Connection
from tworaven_apps.ta2_interfaces.ta2_util import get_grpc_test_json,\
    get_failed_precondition_response


REPLACE_PROBLEM_SCHEMA_FIELD = 'ReplaceProblemSchemaField'

def get_test_info_str():
    """Test data for update_problem_schema call"""
    return '''{"taskType" : "REGRESSION",
     "taskSubtype" : "TASK_SUBTYPE_UNDEFINED",
     "outputType" : "REAL",
     "metric" : "ROOT_MEAN_SQUARED_ERROR"}'''

def update_problem_schema(info_str=None):
    """
    UpdateProblemSchemaRequest={"ReplaceProblemSchemaField":{"metric":"ROC_AUC"}}

    Accept UI input as JSON *string* similar to
     {"taskType" : "REGRESSION",
      "taskSubtype" : "TASK_SUBTYPE_UNDEFINED",
      "outputType" : "REAL",
      "metric" : "ROOT_MEAN_SQUARED_ERROR"}
    """
    if info_str is None:
        info_str = get_test_info_str()

    if info_str is None:
        err_msg = 'UI Str for UpdateProblemSchema is None'
        return get_failed_precondition_response(err_msg)

    # --------------------------------
    # Convert info string to dict
    # --------------------------------
    try:
        info_dict = json.loads(info_str, object_pairs_hook=OrderedDict)
    except json.decoder.JSONDecodeError as err_obj:
        err_msg = 'Failed to convert UI Str to JSON: %s' % (err_obj)
        return get_failed_precondition_response(err_msg)

    # --------------------------------
    # create UpdateProblemSchemaRequest compatible JSON
    # --------------------------------
    if REPLACE_PROBLEM_SCHEMA_FIELD in info_dict:
        info_dict = info_dict[REPLACE_PROBLEM_SCHEMA_FIELD]

    updates_list = []
    for key, val in info_dict.items():
        updates_list.append({key : val})

    final_dict = dict(updates=updates_list)

    content = json.dumps(final_dict)


    # --------------------------------
    # convert the JSON string to a gRPC request
    # --------------------------------
    try:
        req = Parse(content, core_pb2.UpdateProblemSchemaRequest())
    except ParseError as err_obj:
        err_msg = 'Failed to convert JSON to gRPC: %s' % (err_obj)
        return get_failed_precondition_response(err_msg)

    if settings.TA2_STATIC_TEST_MODE:
        return get_grpc_test_json('test_responses/updateproblemschema_ok.json',
                                  dict())


    # --------------------------------
    # Get the connection, return an error if there are channel issues
    # --------------------------------
    core_stub, err_msg = TA2Connection.get_grpc_stub()
    if err_msg:
        return get_failed_precondition_response(err_msg)

    # --------------------------------
    # Send the gRPC request
    # --------------------------------
    try:
        reply = core_stub.UpdateProblemSchema(req)
    except Exception as ex:
        return get_failed_precondition_response(str(ex))

    # --------------------------------
    # Convert the reply to JSON and send it on
    # --------------------------------
    return MessageToJson(reply)


"""
python manage.py shell
#from tworaven_apps.ta2_interfaces.ta2_proxy import *
from tworaven_apps.ta2_interfaces.update_problem_schema import update_problem_schema

updateproblemschema()
"""
