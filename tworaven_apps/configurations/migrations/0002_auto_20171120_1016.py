# -*- coding: utf-8 -*-
# Generated by Django 1.11.4 on 2017-11-20 15:16
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('configurations', '0001_initial'),
    ]

    operations = [
        migrations.AlterField(
            model_name='appconfiguration',
            name='rook_svc_url',
            field=models.CharField(default='/rook-custom/', help_text='URL to the rook server. examples: https://beta.dataverse.org/custom/, http://127.0.0.1:8080/rook-custom/', max_length=255, verbose_name='ROOK_SVC_URL (rook apps)'),
        ),
    ]
