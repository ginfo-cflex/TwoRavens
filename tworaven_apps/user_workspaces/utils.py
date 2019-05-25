"""
Some utils for UserWorkspace calls
"""
from django.conf import settings
from tworaven_apps.utils.basic_response import (ok_resp,
                                                err_resp)

from tworaven_apps.utils.view_helper import \
    (get_request_body, get_request_body_as_json,
     get_json_error,
     get_json_success)
from tworaven_apps.configurations.utils import get_latest_d3m_config
from tworaven_apps.raven_auth.models import User

from tworaven_apps.user_workspaces.models import \
    (UserWorkspace,)
from tworaven_apps.configurations.models_d3m import D3MConfiguration
from tworaven_apps.utils.view_helper import \
    (get_authenticated_user,)

def get_default_workspace_params(**kwargs):
    """Make sure workspace is active"""
    params = dict(is_active=True)

    for key, val, in kwargs.items():
        params[key] = val
    return params



def get_user_workspace_by_id(user_workspace_id):
    """Retrieve a specific UserWorkspace"""
    params = dict(id=user_workspace_id)
    params = get_default_workspace_params(**params)

    try:
        user_ws = UserWorkspace.objects.get(**params)
    except UserWorkspace.DoesNotExist:
        user_msg = 'No active workspaces found for id: %s' % \
                    (user_workspace_id)
        return err_resp(user_msg)

    return ok_resp(user_ws)

def get_user_workspace_config(user, user_workspace_id):
    """Retrieve a specific UserWorkspace"""

    params = dict(user=user,
                  id=user_workspace_id)
    params = get_default_workspace_params(**params)

    try:
        user_ws = UserWorkspace.objects.get(**params)
    except UserWorkspace.DoesNotExist:
        user_msg = 'No workspaces found for user: %s and id: %s' % \
                    (user.username, user_workspace_id)
        return err_resp(user_msg)

    return ok_resp(user_ws)


def get_latest_d3m_user_config_by_request(request):
    """Find the lastest UserWorkspace and return the attached d3m_config"""
    user_info = get_authenticated_user(request)
    if not user_info.success:
        return err_resp(user_info.err_msg)

    user = user_info.result_obj
    return get_latest_d3m_user_config(user)

def get_latest_user_workspace(request):
    """Get latest user workspace"""
    user_info = get_authenticated_user(request)
    if not user_info.success:
        return err_resp(user_info.err_msg)

    user = user_info.result_obj

    params = dict(return_full_workspace=True)
    return get_latest_d3m_user_config(user, create_if_not_found=True, **params)

def get_latest_d3m_user_config(user, create_if_not_found=True, **kwargs):
    """Find the lastest UserWorkspace and return the attached d3m_config

    return_full_workspace = True : return UserWorkspace instead of D3MConfiguration
    """
    if not isinstance(user, User):
        return err_resp('user must be a "User" object, not: "%s"' % user)

    return_full_workspace = kwargs.get('return_full_workspace', False)

    d3m_config = get_latest_d3m_config()
    if not d3m_config:
        return err_resp('No default D3MConfiguration set.')

    params = dict(user=user,
                  is_current_workspace=True,
                  orig_dataset_id=d3m_config.orig_dataset_id)

    params = get_default_workspace_params(**params)

    latest_workspace = UserWorkspace.objects.filter(**params).first()
    if latest_workspace:
        if return_full_workspace:
            return ok_resp(latest_workspace)
        return ok_resp(latest_workspace.d3m_config)

    if create_if_not_found:
        ws_info = create_new_user_workspace(user, d3m_config)
        if not ws_info.success:
            return err_resp('%s (get_latest_d3m_user_config)' %\
                            (ws_info.err_msg))
        new_workspace = ws_info.result_obj
        if return_full_workspace:
            return ok_resp(new_workspace)
        return ok_resp(new_workspace.d3m_config)

    return err_resp('No workspace found for the User and default config')


def create_new_user_workspace(user, d3m_config, **kwargs):
    """Create a new UserWorkspace, making it the current workspace"""
    if not isinstance(user, User):
        return err_resp('"user" is not a User object')
    if not isinstance(d3m_config, D3MConfiguration):
        return err_resp('"d3m_config" is not a D3MConfiguration object')

    params = dict(user=user,
                  is_current_workspace=True,
                  d3m_config=d3m_config,
                  orig_dataset_id=d3m_config.orig_dataset_id)

    params = get_default_workspace_params(**params)

    new_workspace = UserWorkspace(**params)
    new_workspace.save()

    return ok_resp(new_workspace)

def get_user_workspaces_as_dict(user, create_if_not_found=True):
    """Get UserWorkspace list based on the active D3M config, as dicts"""
    ws_info = get_user_workspaces(user, create_if_not_found)
    if not ws_info.success:
        return ws_info

    ws_list = ws_info.result_obj

    ws_list_fmt = [ws.to_dict() for ws in ws_list]

    return ok_resp(ws_list_fmt)

def get_user_workspaces(user, create_if_not_found=True):
    """Get UserWorkspace list based on the active D3M config"""
    if not isinstance(user, User):
        return err_resp('user must be a "User" object, not: "%s"' % user)

    d3m_config = get_latest_d3m_config()
    if not d3m_config:
        return err_resp('No default D3MConfiguration set.')

    params = dict(user=user,
                  orig_dataset_id=d3m_config.orig_dataset_id)

    params = get_default_workspace_params(**params)

    workspaces = UserWorkspace.objects.filter(**params)

    # Make sure the list has a current workspace
    #
    has_current_workspace = [ws for ws in workspaces if ws.is_current_workspace]

    if (not has_current_workspace) or (workspaces.count() == 0):
        if create_if_not_found:
            ws_info = create_new_user_workspace(user, d3m_config)
            if not ws_info.success:
                return err_resp('%s (get_user_workspaces)' %\
                                (ws_info.err_msg))
            return ok_resp([ws_info.result_obj])
        return err_resp('No workspaces found for the User and default config')

    return ok_resp(list(workspaces))

def delete_user_workspaces(user):
    """Used to reset UserWorkspace objects based on a user/problem"""
    if not isinstance(user, User):
        return err_resp('user must be a "User" object, not: "%s"' % user)

    d3m_config = get_latest_d3m_config()
    if not d3m_config:
        return err_resp('No default D3MConfiguration set.')

    params = dict(user=user,
                  orig_dataset_id=d3m_config.orig_dataset_id)

    workspaces = UserWorkspace.objects.filter(**params)
    cnt = workspaces.count()
    workspaces.delete()

    return ok_resp('Workspaces cleared. %d deleted' % cnt)