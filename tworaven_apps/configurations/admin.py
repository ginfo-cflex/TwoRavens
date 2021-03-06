from django.contrib import admin
from tworaven_apps.configurations.models import AppConfiguration
from tworaven_apps.configurations.models_d3m import D3MConfiguration

class AppConfigurationAdmin(admin.ModelAdmin):
    save_on_top = True
    search_fields = ('name',)
    list_editable = ('is_active',)
    list_display = ('name',
                    'is_active',
                    'production',
                    'app_domain',
                    'privacy_mode',
                    'rook_svc_url',
                    'd3m_svc_url',
                    'dataverse_url')
    readonly_fields = ('modified', 'created',)
admin.site.register(AppConfiguration, AppConfigurationAdmin)


class D3MConfigurationAdmin(admin.ModelAdmin):
    save_on_top = True
    search_fields = ('name',)
    list_editable = ('is_default', 'is_selectable_dataset')
    list_filter = ('is_user_config',)
    list_display = ('name',
                    'is_default',
                    'is_selectable_dataset',
                    'is_user_config',
                    'dataset_schema',
                    'problem_schema',
                    'd3m_input_dir',
                    'training_data_root',
                    'modified',
                    'created',)
    readonly_fields = ('slug', 'modified', 'created',
                       'are_paths_valid',
                       'get_bad_paths_for_admin')
admin.site.register(D3MConfiguration, D3MConfigurationAdmin)
#admin.site.register(D3MConfiguration)

"""
from tworaven_apps.configurations.models_d3m import D3MConfiguration
from tworaven_apps.configurations import util_path_check as putil

config = D3MConfiguration.objects.get(pk=2)

info = putil.are_d3m_paths_valid(config)
print('info', info)


info2 = putil.get_bad_paths(config)
print('info', info2)

"""
