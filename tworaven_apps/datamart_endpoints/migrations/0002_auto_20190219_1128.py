# Generated by Django 2.1.5 on 2019-02-19 16:28

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('datamart_endpoints', '0001_initial'),
    ]

    operations = [
        migrations.AlterModelOptions(
            name='datamartinfo',
            options={'ordering': ('name', 'is_active'), 'verbose_name': 'Datamart Connection Information', 'verbose_name_plural': 'Datamart Connection Information'},
        ),
        migrations.AddField(
            model_name='datamartinfo',
            name='documentation_link',
            field=models.URLField(blank=True, help_text='Link to documentation'),
        ),
    ]
