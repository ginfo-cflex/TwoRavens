# Generated by Django 2.0.5 on 2018-08-02 18:56

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('configurations', '0010_auto_20180702_1220'),
    ]

    operations = [
        migrations.AddField(
            model_name='d3mconfiguration',
            name='root_output_directory',
            field=models.TextField(blank=True, help_text='Not an official field.  Used for testing to determine the "/output" directory'),
        ),
    ]
