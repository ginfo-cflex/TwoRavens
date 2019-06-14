from tworaven_apps.behavioral_logs.models import BehavioralLogEntry
from tworaven_apps.behavioral_logs.forms import BehavioralLogEntryForm
from tworaven_apps.behavioral_logs.log_formatter \
    import BehavioralLogFormatter as LogFormatter
from tworaven_apps.behavioral_logs import static_vals as bl_static
from tworaven_apps.raven_auth.models import User

from tworaven_apps.utils.basic_response import (ok_resp,
                                                err_resp)

class LogEntryMaker:
    """util for creating BehavioralLogEntry objects"""
    def __init__(self,):
        pass

    @staticmethod
    def create_system_entry(user, log_data):
        """Add a TA2TA3 entry"""
        assert isinstance(log_data, dict),\
            'log_data must be a python dict. (create_system_entry)'

        log_data['is_optional'] = True

        return LogEntryMaker.create_log_entry(\
                        user,
                        bl_static.ENTRY_TYPE_SYSTEM,
                        log_data)

    @staticmethod
    def create_datamart_entry(user, log_data):
        """Add a TA2TA3 entry"""
        assert isinstance(log_data, dict),\
            'log_data must be a python dict. (create_datamart_entry)'

        print('log_data', log_data)

        return LogEntryMaker.create_log_entry(\
                        user,
                        bl_static.ENTRY_TYPE_DATAMART,
                        log_data)

    @staticmethod
    def create_ta2ta3_entry(user, log_data):
        """Add a TA2TA3 entry"""
        assert isinstance(log_data, dict),\
            'log_data must be a python dict. (create_ta2ta3_entry)'

        return LogEntryMaker.create_log_entry(\
                        user,
                        bl_static.ENTRY_TYPE_TA23API,
                        log_data)


    @staticmethod
    def create_log_entry(user, entry_type, log_data):
        """Add a TA2TA3 entry"""
        if not isinstance(user, User):
            return err_resp("user must be a User object")

        if not isinstance(log_data, dict):
            return err_resp("log_data must be a dict object")

        # set entry type
        log_data['type'] = entry_type

        f = BehavioralLogEntryForm(log_data)
        if not f.is_valid():
            err_msg = 'Log entry params are not valid: %s' % \
                        (dict(f.errors))
            print(f'ERROR!: {err_msg}')
            return err_resp(err_msg)

        new_entry = BehavioralLogEntry(**f.cleaned_data)
        new_entry.user = user

        new_entry.save()

        # user_msg = 'Log entry saved!'

        return ok_resp(new_entry)
