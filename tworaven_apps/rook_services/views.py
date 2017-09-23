import requests
import json
#from io import BytesIO
from datetime import datetime as dt

from requests.exceptions import ConnectionError

from django.conf import settings
from django.shortcuts import render
from django.http import JsonResponse, HttpResponse, HttpResponseRedirect, Http404
from django.views.decorators.csrf import csrf_exempt, csrf_protect

from tworaven_apps.rook_services.models import TestCallCapture
from tworaven_apps.rook_services.rook_app_info import RookAppInfo


ROOK_ZESSIONID = 'zsessionid'
ROOK_FILES_PATH = 'rook-files/'

def view_rook_file_passthrough(request):
    """Redirect rook file requests to rook.
    This is only used in the dev environment!
    In deployment, nginx acts as proxy to these rook files

    http://127.0.0.1:8080/rook-custom/rook-files/data/d3m/o_196seed/preprocess.json
    """

    # start with something like: http://127.0.0.1:8080/rook-custom/rook-files/data/d3m/o_196seed/preprocess.json
    req_file_path = request.get_full_path()
    idx = req_file_path.find(ROOK_FILES_PATH)
    if idx > -1:
        # shorten it to: "rook-files/data/d3m/o_196seed/preprocess.json"
        req_file_path = req_file_path[idx:]

    # doublecheck there's no prepended "/"
    if req_file_path.startswith('/') and len(req_file_path) > 1:
        req_file_path = req_file_path[1:]

    # set the rook url
    rook_server_url = '{0}{1}'.format(settings.R_DEV_SERVER_BASE,
                                      req_file_path)

    # redirect
    return HttpResponseRedirect(rook_server_url)


@csrf_exempt
def view_rook_route(request, app_name_in_url):
    """Route TwoRavens calls to Rook
        orig: TwoRavens -> Rook
        view: TwoRavens -> Django 2ravens -> Rook
    """
    django_session_key = request.session._get_or_create_session_key()

    print('django_session_key', django_session_key)
    # get the app info
    #
    rook_app_info = RookAppInfo.get_appinfo_from_url(app_name_in_url)
    if rook_app_info is None:
        raise Http404('unknown rook app: "{0}" (please add "{0}" to "tworaven_apps/rook_services/app_names.py")'.format(app_name_in_url))

    # look for the "solaJSON" variable in the POST
    #
    if rook_app_info.is_health_check():
        raven_data_text = 'healthcheck'
    elif (not request.POST) or (not 'solaJSON' in request.POST):
        return JsonResponse(dict(status="ERROR", message="solaJSON key not found"))
    else:
        raven_data_text = request.POST['solaJSON']

    # Retrieve post data and attempt to insert django session id
    # (if none exists)
    #
    blank_session_str = '%s":""' % ROOK_ZESSIONID
    if raven_data_text.find(blank_session_str) > -1:
        # was converting to JSON, but now just simple text substitution
        #
        updated_session_str = '%s":"%s"' % (ROOK_ZESSIONID, django_session_key)
        raven_data_text = raven_data_text.replace(blank_session_str, updated_session_str)

    app_data = dict(solaJSON=raven_data_text)

    rook_app_url = rook_app_info.get_rook_server_url()

    # Begin object to capture request
    #
    call_capture = None
    if rook_app_info.record_this_call():
        call_capture = TestCallCapture(\
                        app_name=rook_app_info.name,
                        outgoing_url=rook_app_url,
                        session_id=django_session_key,
                        request=raven_data_text)

    # Call R services
    #
    try:
        r = requests.post(rook_app_url,
                          data=app_data)
    except ConnectionError:
        err_msg = 'R Server not responding: %s' % rook_app_url
        call_capture.add_error_message(err_msg)
        call_capture.save()
        resp_dict = dict(message=err_msg)
        return JsonResponse(resp_dict)

    # Save request result
    #
    if rook_app_info.record_this_call():
        if r.status_code == 200:
            call_capture.add_success_message(r.text, r.status_code)
        else:
            call_capture.add_error_message(r.text, r.status_code)
        call_capture.save()

    # Return the response to the user
    #
    print(40 * '=')
    print(r.text)
    #d = r.json()
    #print(json.dumps(d, indent=4))
    print(r.status_code)

    return HttpResponse(r.text)


NUM_CLICKS_KEY = 'NUM_CLICKS_KEY'

@csrf_exempt
def view_rp_test(request):

    d = dict(name='test url',
             status_code=1)
    return JsonResponse(d)

# example of incoming POST from TwoRavens
"""
<QueryDict: {'solaJSON': ['{"zdata":"fearonLaitinData.tab","zedges":[["country","ccode"],["ccode","cname"]],"ztime":[],"znom":["country"],"zcross":[],"zmodel":"","zvars":["ccode","country","cname"],"zdv":["cname"],"zdataurl":"","zsubset":[["",""],[],[]],"zsetx":[["",""],["",""],["",""]],"zmodelcount":0,"zplot":[],"zsessionid":"","zdatacite":"Dataverse, Admin, 2015, \\"Smoke test\\", http://dx.doi.org/10.5072/FK2/WNCZ16,  Root Dataverse,  V1 [UNF:6:iuFERYJSwTaovVDvwBwsxQ==]","zmetadataurl":"http://127.0.0.1:8080/static/data/fearonLaitin.xml","zusername":"rohit","callHistory":[],"allVars":["durest","aim","casename","ended","ethwar","waryrs","pop","lpop","polity2","gdpen","gdptype","gdpenl","lgdpenl1","lpopl1","region"]}']}>
"""
"""
try:
    # try to convert text to JSON
    #
    raven_data_json = json.loads(request.POST['solaJSON'])

    # Doublecheck that the ROOK_ZESSIONID is blank
    #
    if raven_data_json.get(ROOK_ZESSIONID, None) == '':
        #print('blank session id....')
        # blank id found, subsitute the django session key
        #
        raven_data_json[ROOK_ZESSIONID] = django_session_key
        #
        #
        raven_data_text = json.dumps(raven_data_json)
"""
