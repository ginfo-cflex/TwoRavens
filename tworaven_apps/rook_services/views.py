import requests
from requests.exceptions import ConnectionError

from django.shortcuts import render
from django.conf import settings
from django.http import JsonResponse, HttpResponse, Http404
from django.views.decorators.csrf import csrf_exempt, csrf_protect
from tworaven_apps.rook_services.models import TestCallCapture
from tworaven_apps.rook_services.rook_app_info import RookAppInfo

from datetime import datetime as dt


@csrf_exempt
def view_rook_route(request, app_name_in_url):
    """Route TwoRavens calls to Rook
        orig: TwoRavens -> Rook
        view: TwoRavens -> Django 2ravens -> Rook
    """

    # get the app info
    #
    rook_app_info = RookAppInfo.get_appinfo_from_url(app_name_in_url)
    if rook_app_info is None:
        return Http404('unknown rook app: %s' % app_name_in_url)

    # look for the "solaJSON" variable in the POST
    #
    if (not request.POST) or (not 'solaJSON' in request.POST):
        return JsonResponse(dict(status="ERROR", message="solaJSON key not found"))

    # Retrieve post data
    #
    app_data = dict(solaJSON=request.POST['solaJSON'])

    rook_app_url = rook_app_info.get_rook_server_url()

    # Begin object to capture request
    #
    call_capture = None
    if settings.RECORD_R_SERVICE_ROUTING:
        call_capture = TestCallCapture(\
                        app_name=rook_app_info.name,
                        outgoing_url=rook_app_url,
                        request=request.POST['solaJSON'])

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
    if settings.RECORD_R_SERVICE_ROUTING:
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

    # session test for num clicks
    #
    num_clicks = request.session.get(NUM_CLICKS_KEY, 0)
    num_clicks += 1
    request.session[NUM_CLICKS_KEY] = num_clicks

    print('num_clicks: ', num_clicks)
    print('request.session.session_key: ', request.session.session_key)

    node_length = 'not sent'
    if request.POST:
        node_length = request.POST.get('nodeLength', 'not set by client (err?)')

    if request.user.is_authenticated:
        print('authenticated')
        # Do something for authenticated users.

    else:
        print('anonymous')

    user_msg = ('\nnode length: {1}. hello ({0})').format(\
                    dt.now(),
                    node_length)

    d = dict(status='ok',
             data=dict(\
                 num_clicks=num_clicks,
                 node_length=node_length,
                 server_time='%s' % dt.now()),
             message=user_msg)

    return JsonResponse(d)

# example of incoming POST from TwoRavens
"""
<QueryDict: {'solaJSON': ['{"zdata":"fearonLaitinData.tab","zedges":[["country","ccode"],["ccode","cname"]],"ztime":[],"znom":["country"],"zcross":[],"zmodel":"","zvars":["ccode","country","cname"],"zdv":["cname"],"zdataurl":"","zsubset":[["",""],[],[]],"zsetx":[["",""],["",""],["",""]],"zmodelcount":0,"zplot":[],"zsessionid":"","zdatacite":"Dataverse, Admin, 2015, \\"Smoke test\\", http://dx.doi.org/10.5072/FK2/WNCZ16,  Root Dataverse,  V1 [UNF:6:iuFERYJSwTaovVDvwBwsxQ==]","zmetadataurl":"http://127.0.0.1:8080/static/data/fearonLaitin.xml","zusername":"rohit","callHistory":[],"allVars":["durest","aim","casename","ended","ethwar","waryrs","pop","lpop","polity2","gdpen","gdptype","gdpenl","lgdpenl1","lpopl1","region"]}']}>
"""
