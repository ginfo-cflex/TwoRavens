import requests
from django.urls import reverse

from django.shortcuts import render
from django.http import JsonResponse, HttpResponse, Http404
from django.conf import settings

from tworaven_apps.api_docs.forms import ClientTestForm
from tworaven_apps.ta2_interfaces.models import GRPC_JSON_KEY

# Create your views here.
def view_test_form(request):
    """View test form"""

    info_dict = {}

    if request.POST:

        client_form = ClientTestForm(request.POST)
        if client_form.is_valid():
            content = client_form.cleaned_data['content']
            la_url = '%s://%s%s' % \
                    (settings.SERVER_SCHEME,
                     request.get_host(),
                     reverse('view_startsession', args=()))
            resp_text = make_request(la_url, content)
            return HttpResponse(resp_text)
        else:
            info_dict['form_errs'] = client_form.errors
            client_form = ClientTestForm()
    else:
        client_form = ClientTestForm()

    info_dict['cform'] = client_form

    return render(request,
                  'api_docs/test_form.html',
                  info_dict)


def make_request(la_url, content):

    payload = {GRPC_JSON_KEY : content}

    print('payload', payload)
    resp = requests.post(la_url, data=payload)

    print(resp.text)
    print(resp.status_code)

    return resp.text
