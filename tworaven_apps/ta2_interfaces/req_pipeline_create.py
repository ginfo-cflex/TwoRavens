import json
from os.path import dirname, isfile, join, abspath
from collections import OrderedDict

from google.protobuf.json_format import MessageToJson,\
    Parse, ParseError
from django.conf import settings

from tworaven_apps.ta2_interfaces import core_pb2
from tworaven_apps.ta2_interfaces.ta2_connection import TA2Connection
from tworaven_apps.ta2_interfaces.ta2_util import get_grpc_test_json,\
    get_failed_precondition_response
from tworaven_apps.utils.csv_to_json import convert_csv_file_to_json


PIPELINE_CREATE_REQUEST = 'PipelineCreateRequest'

def get_test_info_str():
    """Test data for update_problem_schema call"""
    return """{"context": {"sessionId": "session_0"}, "trainFeatures": [{"featureId": "cylinders", "dataUri": "data/d3m/o_196seed/data/trainDatamerged.tsv"}, {"featureId": "cylinders", "dataUri": "data/d3m/o_196seed/data/trainDatamerged.tsv"}], "task": "REGRESSION", "taskSubtype": "UNIVARIATE", "output": "REAL", "metrics": ["ROOT_MEAN_SQUARED_ERROR"], "targetFeatures": [{"featureId": "class", "dataUri": "data/d3m/o_196seed/data/trainDatamerged.tsv"}], "maxPipelines": 10"""

def pipeline_create(info_str=None):
    """Send the pipeline create request via gRPC"""
    if info_str is None:
        info_str = get_test_info_str()

    if info_str is None:
        err_msg = 'UI Str for %s is None' % PIPELINE_CREATE_REQUEST
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
    # convert the JSON string to a gRPC request
    # --------------------------------
    try:
        req = Parse(info_str, core_pb2.PipelineCreateRequest())
    except ParseError as err_obj:
        err_msg = 'Failed to convert JSON to gRPC: %s' % (err_obj)
        return get_failed_precondition_response(err_msg)

    if settings.TA2_STATIC_TEST_MODE:

        template_info = get_predict_file_info_dict(info_dict.get('task'))

        return get_grpc_test_json('test_responses/createpipeline_ok.json',
                                  template_info)

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
        reply = core_stub.CreatePipelines(req)
    except Exception as ex:
        return get_failed_precondition_response(str(ex))

    print('reply', reply)
    try:
        print(MessageToJson(reply))
    except:
        print('failed unary convert to JSON')
    # --------------------------------
    # Convert the reply to JSON and send it on
    # --------------------------------
    results = map(MessageToJson, reply)
    result_str = '['+', '.join(results)+']'


    return result_str


def get_predict_file_info_dict(task_type, cnt=1):
    """Create the file uri and embed the file content"""

    test_dirpath = join(dirname(abspath(__file__)),
                         'templates',
                         'test_responses',
                         'files')

    json_str = None
    err_found = False
    if task_type == 'REGRESSION':
        fpath = join(test_dirpath, 'samplePredReg.csv')

    elif task_type == 'CLASSIFICATION':

        fpath = join(test_dirpath, 'models.csv')
    else:
        err_found = True
        fpath = '(no sample file for this task type: %s)' % task_type

    if not isfile(fpath):
        if not err_found:
            fpath = 'Error creating uri.  File not found: %s' % fpath
    else:
        json_str, err_msg = convert_csv_file_to_json(fpath)


    dinfo = dict(FILE_URI=fpath)
    if json_str:
        dinfo['FILE_CONTENT'] = """ "file_%s" : %s """ % \
                                (cnt, json_str)

    return dinfo


"""
python manage.py shell
#from tworaven_apps.ta2_interfaces.ta2_proxy import *
from tworaven_apps.ta2_interfaces.update_problem_schema import update_problem_schema

updateproblemschema()
"""
