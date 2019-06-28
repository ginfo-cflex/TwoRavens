# Generated by Django 2.1.9 on 2019-06-19 21:34

from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    dependencies = [
        ('user_workspaces', '0005_auto_20190617_1640'),
    ]

    operations = [
        migrations.AddField(
            model_name='userworkspace',
            name='started_as_shared_workspace',
            field=models.BooleanField(default=False, help_text='auto-filled on save'),
        ),
        migrations.AlterField(
            model_name='userworkspace',
            name='original_workspace',
            field=models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, related_name='orig_workspace+', to='user_workspaces.UserWorkspace'),
        ),
        migrations.AlterField(
            model_name='userworkspace',
            name='previous_workspace',
            field=models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, related_name='prev_workspace+', to='user_workspaces.UserWorkspace'),
        ),
    ]