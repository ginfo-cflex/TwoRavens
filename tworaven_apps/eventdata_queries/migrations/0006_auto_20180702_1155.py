# Generated by Django 2.0.5 on 2018-07-02 15:55

from django.db import migrations


class Migration(migrations.Migration):

    dependencies = [
        ('eventdata_queries', '0005_auto_20180702_1154'),
    ]

    operations = [
        migrations.AlterModelOptions(
            name='eventdatasavedquery',
            options={'ordering': ('-created',), 'verbose_name_plural': 'Event data saved queries'},
        ),
    ]
