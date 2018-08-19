import json, jsonfield
from collections import OrderedDict
from django import forms
from django.conf import settings
from tworaven_apps.eventdata_queries.models import (EventDataSavedQuery, ArchiveQueryJob)
from tworaven_apps.eventdata_queries.models import (AGGREGATE, SUBSET, TYPE_OPTIONS, TYPE_CHOICES, METHOD_CHOICES, HOST_CHOICES)

class EventDataSavedQueryForm(forms.Form):
    """ form for event data queries"""

    name = forms.CharField(required=True, label='Name')
    description = forms.CharField(widget=forms.Textarea)
    username = forms.CharField(required=True, label='UserName')
    query = forms.CharField(widget=forms.Textarea)
    result_count = forms.IntegerField(required=True, label='result_count')
    collection_name = forms.CharField(widget=forms.Textarea)
    collection_type = forms.CharField(required=True, initial=SUBSET)
    save_to_dataverse = forms.BooleanField(required=False)

    def clean_name(self):
        name = self.cleaned_data.get('name')

        return name

    def clean_username(self):
        username = self.cleaned_data.get('username')

        return username

    def clean_description(self):
        desc = self.cleaned_data.get('description')

        return desc

    def clean_query(self):
        """
        - passed to the form as a string but it we want it as an OrderedDict
            - e.g. input: "OrderedDict([('ads', 'asd')])"
        """
        query_str = self.cleaned_data.get('query')

        try:
            dict_type = eval(query_str)
        except SyntaxError:
            raise forms.ValidationError("The query is invalid: %s" % query_str)
        except NameError:
            raise forms.ValidationError("The query is invalid: %s" % query_str)

        if isinstance(dict_type, dict):
            print("dict type ")
            return dict_type
        else:
            try:
                print("non dict type")
                non_dict_type = json.dumps(query_str)
                return non_dict_type
            except SyntaxError:
                raise forms.ValidationError("The query is invalid and non_dict type: %s" % query_str)
            except NameError:
                raise forms.ValidationError("The query is invalid and non dict type: %s" % query_str)

    def clean_result_count(self):
        res_count = self.cleaned_data.get('result_count')

        return res_count

    def clean_dataset(self):
        dataset_input = self.cleaned_data.get('collection_name')

        return dataset_input

    def clean_dataset_type(self):
        dataset_type = self.cleaned_data.get('collection_type')
        print("type choces", list(TYPE_OPTIONS))
        if dataset_type in TYPE_OPTIONS:
            return dataset_type
        else:
            raise forms.ValidationError("The type input is not among subset or aggregate: %s" % dataset_type)


    def clean_save_to_dataverse(self):
        save_to_dataverse = self.cleaned_data.get('save_to_dataverse')

        if not save_to_dataverse:
            save_to_dataverse = False

        return save_to_dataverse


class EventDataQueryFormSearch(forms.Form):
    """ to check if search parameters are ok"""

    name = forms.CharField(required=False, label='Name')
    description = forms.CharField(required=False, widget=forms.Textarea)
    username = forms.CharField(required=False, label='UserName')

    def clean_name(self):
        name = self.cleaned_data.get('name')

        return name

    def clean_username(self):
        username = self.cleaned_data.get('username')

        return username

    def clean_description(self):
        desc = self.cleaned_data.get('description')

        return desc


class EventDataGetDataForm(forms.Form):
    """ check if query submission parameters are ok"""

    host = forms.CharField(required=False, widget=forms.Textarea, initial=HOST_CHOICES[0])
    dataset = forms.CharField(required=True, widget=forms.Textarea)
    method = forms.CharField(required=True, widget=forms.Textarea)
    query = forms.CharField(required=True, widget=forms.Textarea)
    distinct = forms.CharField(required=False, widget=forms.Textarea)

    def clean_host(self):
        host = self.cleaned_data.get('host')
        if host in HOST_CHOICES:
            return host
        else:
            raise forms.ValidationError('The host is not among %s: %s' % (str(HOST_CHOICES), host))

    def clean_dataset(self):
        return self.cleaned_data.get('dataset')

    def clean_method(self):
        method = self.cleaned_data.get('method')
        if method in METHOD_CHOICES:
            return method
        else:
            raise forms.ValidationError("The collection method is not among %s: %s" % (str(METHOD_CHOICES), method))

    def clean_query(self):
        print("Query")
        print(self.cleaned_data.get('query'))
        return json.loads(self.cleaned_data.get('query'))

    def clean_distinct(self):
        return self.cleaned_data.get('distinct')


class EventDataGetMetadataForm(forms.Form):
    """ check if metadata parameters are ok"""

    alignments = forms.CharField(required=False, widget=forms.Textarea)
    formats = forms.CharField(required=False, widget=forms.Textarea)
    datasets = forms.CharField(required=False, widget=forms.Textarea)

    def clean_alignments(self):
        return self.cleaned_data.get('alignments')

    def clean_formats(self):
        return self.cleaned_data.get('formats')

    def clean_datasets(self):
        return self.cleaned_data.get('datasets')

