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
SECRET_KEY = '&1p0bkm1!)x49#g^fcqlwa8ds_p_r$x@c*+vrpveaq=dhr_rzu'

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

    'tworaven_apps.raven_auth', # user model
    'tworaven_apps.workspaces', # save session state

    'tworaven_apps.configurations', # UI domain/mode configuration
    'tworaven_apps.ta2_interfaces', # sending UI through to TA2 and back again
    'tworaven_apps.content_pages',
    'tworaven_apps.rook_services', # sending UI calls to rook and back again
    'tworaven_apps.api_docs',
    'tworaven_apps.call_captures', # capture data sent from UI out to rook/TA2
    'tworaven_apps.eventdata_queries', # eventdata API services

    # webpack!
    'webpack_loader',
]

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

LOGIN_REDIRECT_URL = 'home'

TEMPLATES = [
    {
        'BACKEND': 'django.template.backends.django.DjangoTemplates',
        'DIRS': [join(BASE_DIR, 'templates'),],
        'APP_DIRS': True,
        'OPTIONS': {
            'context_processors': [
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

LOGIN_URL = reverse_lazy('home')    #'/auth/login/'

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
# D3M - TA2 settings
# ---------------------------
TA2_STATIC_TEST_MODE = strtobool(os.environ.get('TA2_STATIC_TEST_MODE', 'True'))   # True: canned responses
TA2_TEST_SERVER_URL = os.environ.get('TA2_TEST_SERVER_URL', 'localhost:45042')
TA3_GPRC_USER_AGENT = os.environ.get('TA3_GPRC_USER_AGENT', 'tworavens')

TA2_GPRC_SHORT_TIMEOUT = 3 # seconds
TA2_GPRC_LONG_TIMEOUT = 5 * 60 # 5 minutes

# D3M - gRPC file uris
MAX_EMBEDDABLE_FILE_SIZE = .5 * 500000

SWAGGER_HOST = '127.0.0.1:8080'



# ---------------------------
# REDIS/CELERY SETTINGS
# ---------------------------
REDIS_HOST = os.environ.get('REDIS_HOST', 'localhost')
REDIS_PORT = os.environ.get('REDIS_PORT', 6379)

CELERY_BROKER_URL = 'redis://%s:%d' % (REDIS_HOST, REDIS_PORT)
CELERY_RESULT_BACKEND = 'redis://%s:%d' % (REDIS_HOST, REDIS_PORT)

# ---------------------------
#  For depositing Dataverse data
# ---------------------------
DATAVERSE_SERVER = os.environ.get('DATAVERSE_SERVER', 'https://dataverse.harvard.edu')
DATAVERSE_API_KEY = os.environ.get('DATAVERSE_API_KEY', '623d7768-fa3d-42c5-8a03-16d473dc8953')
DATASET_PERSISTENT_ID = os.environ.get('DATASET_PERSISTENT_ID', 'doi%3A10.7910%2FDVN%2FSJWX4S')

