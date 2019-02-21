"""
Django settings for tworavensproject project.

Generated by 'django-admin startproject' using Django 1.11.4.

For more information on this file, see
https://docs.djangoproject.com/en/1.11/topics/settings/

For the full list of settings and their values, see
https://docs.djangoproject.com/en/1.11/ref/settings/
"""
import os
import sys
from os.path import abspath, dirname, join
from distutils.util import strtobool

from django.urls import reverse_lazy

# Build paths inside the project like this: os.path.join(BASE_DIR, ...)
BASE_DIR = dirname(dirname(dirname(abspath(__file__))))

# -----------------------------------------------------
# Link to copy of the TA3TA2 API
# https://gitlab.com/datadrivendiscovery/ta3ta2-api
# -----------------------------------------------------
TA3TA2_API_DIR = join(BASE_DIR, 'submodules', 'ta3ta2-api')
sys.path.append(TA3TA2_API_DIR)

TWORAVENS_COMMON_DIR = join(BASE_DIR, 'assets', 'common')
sys.path.append(TWORAVENS_COMMON_DIR)

# -----------------------------------------------------
# Link to copy of the raven-metadata-service
# for the preprocess script
#
# https://github.com/TwoRavens/raven-metadata-service
# -----------------------------------------------------
#RAVEN_METADATA_SVC = join(BASE_DIR, 'submodules', 'raven-metadata-service')
#RAVEN_PREPROCESS = join(RAVEN_METADATA_SVC, 'preprocess', 'code')
#sys.path.append(RAVEN_PREPROCESS)


# Quick-start development settings - unsuitable for production
# See https://docs.djangoproject.com/en/1.11/howto/deployment/checklist/

# SECURITY WARNING: keep the secret key used in production secret!
SECRET_KEY = os.environ.get(\
                'SECRET_KEY',
                'please-set-a-secret-secret-key')

# SECURITY WARNING: don't run with debug turned on in production!
DEBUG = True

SITE_ID = 1

AUTH_USER_MODEL = 'raven_auth.User'

# Application definition
INSTALLED_APPS = [
    'django.contrib.admin',
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.messages',
    'django.contrib.sites',
    'django.contrib.humanize',
    'django.contrib.staticfiles',

    'channels', # django channels

    'tworaven_apps.websocket_views', # websocket support

    'social_django',    # social auth
    'tworaven_apps.raven_auth', # user model
    'tworaven_apps.user_workspaces', # save session state
    'tworaven_apps.workspaces', # save session state

    'tworaven_apps.configurations', # UI domain/mode configuration
    'tworaven_apps.ta2_interfaces', # sending UI through to TA2 and back again
    'tworaven_apps.content_pages',
    'tworaven_apps.rook_services', # sending UI calls to rook and back again
    'tworaven_apps.api_docs',
    'tworaven_apps.call_captures', # capture data sent from UI out to rook/TA2
    'tworaven_apps.eventdata_queries', # eventdata API services
    'tworaven_common_apps.datamart_endpoints', # Datamart connections

    # webpack!
    'webpack_loader',
]

# Channels
ASGI_APPLICATION = "tworavensproject.routing.application"
CHANNEL_LAYERS = {
    'default': {
        'BACKEND': 'channels_redis.core.RedisChannelLayer',
        'CONFIG': {
            "hosts": [('127.0.0.1', 6379)],
        },
    },
}

# WEBSOCKET PREFIX
# specify whether over a regular (ws://)
# or secure connection (ws://)
WEBSOCKET_PREFIX = os.environ.get('WEBSOCKET_PREFIX', 'ws://')
assert WEBSOCKET_PREFIX in ('ws://', 'wss://'), \
    "Django settings error: 'WEBSOCKET_PREFIX' must be set to 'ws://' or 'wss://'"

MIDDLEWARE = [
    'django.middleware.security.SecurityMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.common.CommonMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    'django.middleware.clickjacking.XFrameOptionsMiddleware',
    'django.middleware.locale.LocaleMiddleware',
]

ROOT_URLCONF = 'tworavensproject.urls'

LOGIN_URL = reverse_lazy('home')    #'/auth/login/'

LOGIN_REDIRECT_URL = 'home'

TEMPLATES = [
    {
        'BACKEND': 'django.template.backends.django.DjangoTemplates',
        'DIRS': [join(BASE_DIR, 'templates'),],
        'APP_DIRS': True,
        'OPTIONS': {
            'context_processors': [
                # start: social auth
                'social_django.context_processors.backends',
                'social_django.context_processors.login_redirect',
                # end: social auth
                'django.template.context_processors.debug',
                'django.template.context_processors.request',
                'django.contrib.auth.context_processors.auth',
                'django.contrib.messages.context_processors.messages',
            ],
        },
    },
]

WSGI_APPLICATION = 'tworavensproject.wsgi.application'


# Database
# https://docs.djangoproject.com/en/1.11/ref/settings/#databases

DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.sqlite3',
        'NAME': os.path.join(BASE_DIR, 'db.sqlite3'),
    }
}

# -------------------------------
# Start: Social Auth
# - Added 8/2018
# https://python-social-auth.readthedocs.io/en/latest/configuration/django.html
# -------------------------------
ALLOW_SOCIAL_AUTH = strtobool(os.environ.get('ALLOW_SOCIAL_AUTH', 'False'))

AUTHENTICATION_BACKENDS = (
    'social_core.backends.github.GithubOAuth2',
    #'social_core.backends.google.GoogleOpenId',
    #'social_core.backends.google.GoogleOAuth2',
    #'social_core.backends.google.GoogleOAuth',
    #'social_core.backends.twitter.TwitterOAuth',
    #'social_core.backends.yahoo.YahooOpenId',
    'django.contrib.auth.backends.ModelBackend',
)
SOCIAL_AUTH_URL_NAMESPACE = 'social'

xSOCIAL_AUTH_PIPELINE = (
    'social_core.pipeline.social_auth.social_details',
    'social_core.pipeline.social_auth.social_uid',
    'social_core.pipeline.social_auth.social_user',
    'social_core.pipeline.user.get_username',
    'social_core.pipeline.user.create_user',
    'social_core.pipeline.social_auth.associate_user',
    'social_core.pipeline.social_auth.load_extra_data',
    'social_core.pipeline.user.user_details',
    'social_core.pipeline.social_auth.associate_by_email',
)

SOCIAL_AUTH_GITHUB_KEY = os.environ.get('SOCIAL_AUTH_GITHUB_KEY', 'not-set')
SOCIAL_AUTH_GITHUB_SECRET = os.environ.get('SOCIAL_AUTH_GITHUB_SECRET', 'not-set')

SOCIAL_AUTH_GITHUB_AUTH_EXTRA_ARGUMENTS = dict()
#SOCIAL_AUTH_GOOGLE_OAUTH2_KEY = os.environ.get(\
#                            'SOCIAL_AUTH_GOOGLE_OAUTH2_KEY', 'not-set')
#SOCIAL_AUTH_GOOGLE_OAUTH2_SECRET = os.environ.get(\
#                            'SOCIAL_AUTH_GOOGLE_OAUTH2_SECRET', 'not-set')

# -------------------------------
# End: Social Auth
# -------------------------------

# Password validation
# https://docs.djangoproject.com/en/1.11/ref/settings/#auth-password-validators

AUTH_PASSWORD_VALIDATORS = [
    dict(NAME='django.contrib.auth.password_validation.UserAttributeSimilarityValidator'),
    dict(NAME='django.contrib.auth.password_validation.MinimumLengthValidator'),
    dict(NAME='django.contrib.auth.password_validation.CommonPasswordValidator'),
    dict(NAME='django.contrib.auth.password_validation.NumericPasswordValidator'),
]



# Internationalization
# https://docs.djangoproject.com/en/1.11/topics/i18n/

LANGUAGE_CODE = 'en-us'

TIME_ZONE = 'UTC'

USE_I18N = True

USE_L10N = True

USE_TZ = True

# Store the CSRF in a session: https://docs.djangoproject.com/en/1.11/ref/settings/#std:setting-CSRF_USE_SESSIONS
#CSRF_USE_SESSIONS = True
CSRF_COOKIE_NAME = 'CSRF_2R'

# Static files (CSS, JavaScript, Images)
# https://docs.djangoproject.com/en/1.11/howto/static-files/


STATIC_URL = '/static/'

STATICFILES_DIRS = [join(BASE_DIR, 'assets')]


RECORD_R_SERVICE_ROUTING = False # log R service requests/response JSON to db
RECORD_D3M_SERVICE_ROUTING = False # log D3M service requests

PAGE_CACHE_TIME = 60 * 60 * 2 # 2 hours

WEBPACK_LOADER = {
    'DEFAULT': {
        'CACHE': not DEBUG,
        'BUNDLE_DIR_NAME': 'build/', # must end with slash
        'STATS_FILE': join(BASE_DIR, 'webpack-stats.json'),
        'POLL_INTERVAL': 0.1,
        'TIMEOUT': None,
        'IGNORE': ['.+\.hot-update.js', '.+\.map']
    }
}

SESSION_SAVE_EVERY_REQUEST = True

SERVER_SCHEME = 'http'  # or https



# ---------------------------
# D3M - Config Settings - started winter 2019
# ---------------------------
# This switches between 2019 config (True)
# and the older "search_config.json" file
#
D3M_USE_2019_CONFIG = strtobool(os.environ.get('D3M_USE_2019_CONFIG', 'True'))

# D3MRUN - A label what is the setting under which the pod is being run; possible values: ta2, ta2ta3; this variable is available only for informative purposes but it is not used anymore to change an overall mode of operation of TA2 system because now TA2 evaluation will happen through TA2-TA3 API as well
D3MRUN = os.environ.get('D3MRUN', 'ta2ta3')

# D3MINPUTDIR - a location of dataset(s), can contain multiple datasets in arbitrary directory structure, read-only
D3MINPUTDIR = os.environ.get('D3MINPUTDIR', None)

# D3MPROBLEMPATH - a location to problem description to use (should be under D3MINPUTDIR), datasets are linked from the problem description using IDs, those datasets should exist inside D3MINPUTDIR
D3MPROBLEMPATH = os.environ.get('D3MPROBLEMPATH', None)

# D3MOUTPUTDIR - a location of output files, shared by TA2 and TA3 pods (and probably data mart)
D3MOUTPUTDIR = os.environ.get('D3MOUTPUTDIR', None)

# D3MLOCALDIR - a local-to-host directory provided; used by memory sharing mechanisms
D3MLOCALDIR = os.environ.get('D3MLOCALDIR', None)

# D3MSTATICDIR - a path to the volume with primitives' static files
D3MSTATICDIR = os.environ.get('D3MSTATICDIR', None)

# D3MCPU - available CPU units in Kubernetes specification
D3MCPU = os.environ.get('D3MCPU', None)

# D3MRAM - available memory units in Kubernetes specification
D3MRAM = os.environ.get('D3MRAM', None)

# D3MTIMEOUT - time limit for the search phase (available to the pod), in seconds
D3MTIMEOUT = os.environ.get('D3MTIMEOUT', None)


# ---------------------------
# D3M - TA2 settings
# ---------------------------
TA2_STATIC_TEST_MODE = strtobool(os.environ.get('TA2_STATIC_TEST_MODE', 'True'))   # True: canned responses
TA2_TEST_SERVER_URL = os.environ.get('TA2_TEST_SERVER_URL', 'localhost:45042')
TA3_GRPC_USER_AGENT = os.environ.get('TA3_GRPC_USER_AGENT', 'TwoRavens')

# for non-streaming responses
TA2_GRPC_FAST_TIMEOUT = os.environ.get('TA2_GRPC_FAST_TIMEOUT', 10) # seconds
TA2_GRPC_SHORT_TIMEOUT = os.environ.get('TA2_GRPC_SHORT_TIMEOUT', 60) # seconds

# for streaming responses
TA2_GRPC_LONG_TIMEOUT = os.environ.get('TA2_GRPC_LONG_TIMEOUT', 8 * 60)  # 8 minutes

# D3M - gRPC file uris
MAX_EMBEDDABLE_FILE_SIZE = 1 * 500000

SWAGGER_HOST = '127.0.0.1:8080'

# Delete saved model objects via fab commands
#
ALLOW_FAB_DELETE = False

# ---------------------------
# REDIS/CELERY SETTINGS
# ---------------------------
REDIS_HOST = os.environ.get('REDIS_HOST', 'localhost')
REDIS_PORT = os.environ.get('REDIS_PORT', 6379)

CELERY_BROKER_URL = 'redis://%s:%d' % (REDIS_HOST, REDIS_PORT)
CELERY_RESULT_BACKEND = 'redis://%s:%d' % (REDIS_HOST, REDIS_PORT)

# ---------------------------
# EventData: depositing Dataverse data
# ---------------------------
DATAVERSE_SERVER = os.environ.get('DATAVERSE_SERVER', 'https://demo.dataverse.org')
DATAVERSE_API_KEY = os.environ.get('DATAVERSE_API_KEY', 'Get API Key from dataverse')
DATASET_PERSISTENT_ID = os.environ.get('DATASET_PERSISTENT_ID', 'doi:10.5072/FK2/BGPZC3')

# -----------------------------------------
# Mongo connection string
# - Takes precedence over other Mongo creds,
#   including the settings with "EVENTDATA_"
# -----------------------------------------
MONGO_CONNECTION_STRING = os.environ.get('MONGO_CONNECTION_STRING', '')


# database for storing manipulations
TWORAVENS_MONGO_DB_NAME = os.environ.get('TWORAVENS_MONGO_DB_NAME', 'tworavens')
MONGO_COLLECTION_PREFIX = 'tr_'  # mongo collection names may not start with a number

# -------------------------
# EventData: mongo related
# -------------------------
EVENTDATA_PRODUCTION_MODE = os.environ.get('EVENTDATA_PRODUCTION_MODE', "no") == "yes"

EVENTDATA_MONGO_DB_ADDRESS = os.environ.get('EVENTDATA_MONGO_DB_ADDRESS', '127.0.0.1:27017')

EVENTDATA_MONGO_USERNAME = os.environ.get('EVENTDATA_MONGO_USERNAME', '')
EVENTDATA_MONGO_PASSWORD = os.environ.get('EVENTDATA_MONGO_PASSWORD', '')

EVENTDATA_PHOENIX_SERVER_ADDRESS = 'http://149.165.156.33:5002/api/data?'
EVENTDATA_PRODUCTION_SERVER_ADDRESS = os.environ.get('EVENTDATA_PRODUCTION_SERVER_ADDRESS', EVENTDATA_PHOENIX_SERVER_ADDRESS)

# API KEY, Load from ENV variable.  If it doesn't exist, use the default
EVENTDATA_DEFAULT_API_KEY = 'api_key=CD75737EF4CAC292EE17B85AAE4B6'
EVENTDATA_SERVER_API_KEY = os.environ.get('EVENTDATA_SERVER_API_KEY', EVENTDATA_DEFAULT_API_KEY)
EVENTDATA_DB_NAME = os.environ.get('EVENTDATA_DB_NAME', 'event_data')


# -------------------------
# Datamart related
# -------------------------
DATAMART_SHORT_TIMEOUT = 10 # seconds
DATAMART_LONG_TIMEOUT = 30 # seconds
