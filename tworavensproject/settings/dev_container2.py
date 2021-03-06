from .local_settings import *
from distutils.util import strtobool
import os


R_DEV_SERVER_BASE = os.environ.get('R_DEV_SERVER_BASE',
                                   'http://0.0.0.0:8000/')

TEST_DIRECT_STATIC = STATIC_ROOT

WEBPACK_LOADER['DEFAULT'].update(\
    dict(BUNDLE_DIR_NAME='dist/',
         STATS_FILE=join(BASE_DIR, 'webpack-stats-prod.json'))\
    )

RECORD_R_SERVICE_ROUTING = True # log R service requests/response JSON to db

TA2_STATIC_TEST_MODE = strtobool(\
                        os.environ.get('TA2_STATIC_TEST_MODE',
                                       'False'))   # 'True': canned responses

TA2_TEST_SERVER_URL = os.environ.get('TA2_TEST_SERVER_URL',
                                     'localhost:45042') # 'localhost:45042'

TA3_GRPC_USER_AGENT = os.environ.get('TA3_GRPC_USER_AGENT',
                                     'tworavens')


SESSION_COOKIE_NAME = os.environ.get('SESSION_COOKIE_NAME',
                                     'tworavens_deploy')

CSRF_COOKIE_NAME = os.environ.get('CSRF_COOKIE_NAME',
                                  'tworavens_deploy_csrf')


SWAGGER_HOST = '127.0.0.1:80'

# -----------------------------------
# staticfiles served via nginx
# -----------------------------------
# -----------------------------------
# staticfiles served via nginx
# -----------------------------------
DEFAULT_STATIC_ROOT = join('/ravens_volume', 'staticfiles', 'static')
STATIC_ROOT = os.environ.get('STATIC_ROOT',
                             DEFAULT_STATIC_ROOT)
if not os.path.isdir(STATIC_ROOT):
    os.makedirs(STATIC_ROOT)
