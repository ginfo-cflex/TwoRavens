# -*- coding: utf-8 -*-
# Generated by Django 1.11.4 on 2017-12-19 04:24
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('configurations', '0006_auto_20171127_1707'),
    ]

    operations = [
        migrations.AddField(
            model_name='d3mconfiguration',
            name='cpus',
            field=models.IntegerField(default=-1, help_text='Number of cpus available for search.'),
        ),
        migrations.AddField(
            model_name='d3mconfiguration',
            name='problem_root',
            field=models.TextField(blank=True, help_text='Path to the root directory of the problem.'),
        ),
        migrations.AddField(
            model_name='d3mconfiguration',
            name='ram',
            field=models.CharField(blank=True, help_text='Amount of RAM available for search.', max_length=255),
        ),
        migrations.AddField(
            model_name='d3mconfiguration',
            name='timeout',
            field=models.IntegerField(default=-1, help_text='Allotted time for search, in minutes. No timeout if negative.'),
        ),
    ]