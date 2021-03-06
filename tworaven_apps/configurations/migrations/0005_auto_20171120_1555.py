# -*- coding: utf-8 -*-
# Generated by Django 1.11.4 on 2017-11-20 20:55
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('configurations', '0004_remove_appconfiguration_d3m_mode'),
    ]

    operations = [
        migrations.AlterField(
            model_name='appconfiguration',
            name='app_domain',
            field=models.CharField(choices=[('D3M_DOMAIN', 'D3M_DOMAIN'), ('DATAVERSE_DOMAIN', 'DATAVERSE_DOMAIN'), ('EVENTDATA_DOMAIN', 'EVENTDATA_DOMAIN')], max_length=70, verbose_name='.js variable "APP_DOMAIN"'),
        ),
        migrations.AlterField(
            model_name='appconfiguration',
            name='d3m_svc_url',
            field=models.CharField(default='/d3m-service/', help_text='URL used to make calls that are converted to gRPC messages and sent to D3M applications', max_length=255, verbose_name='.js variable "D3M_SVC_URL"'),
        ),
        migrations.AlterField(
            model_name='appconfiguration',
            name='dataverse_url',
            field=models.URLField(help_text='URL to Dataverseexamples: https://beta.dataverse.org,https://dataverse.harvard.edu', verbose_name='.js variable "DATAVERSE_URL"'),
        ),
        migrations.AlterField(
            model_name='appconfiguration',
            name='production',
            field=models.BooleanField(help_text=' True -> data, metadata from live server resources instead of local versions', verbose_name='.js variable "PRODUCTION".'),
        ),
        migrations.AlterField(
            model_name='appconfiguration',
            name='rook_svc_url',
            field=models.CharField(default='/rook-custom/', help_text='URL to the rook server. examples: https://beta.dataverse.org/custom/, http://127.0.0.1:8080/rook-custom/', max_length=255, verbose_name='.js variable "ROOK_SVC_URL"'),
        ),
    ]
