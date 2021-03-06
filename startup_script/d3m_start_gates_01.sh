#!/bin/bash

# --------------------------------------------------------------
# The sqlite database will be in a different place--not the volume
# --------------------------------------------------------------
cd /var/webapps/TwoRavens

printf "\n(5) Run fab setup commands\n"

# Create new db
#
fab init_db
fab check_datamarts

fab set_2ravens_public_site

# Make superuser and test user
#
fab create_django_superuser
fab create_test_user

# Load an initial config
#
fab load_docker_ui_config

# collect static files
#
fab collect_static


# -------------------------------
# Start-up server on D3M shared volume
# -------------------------------
cd /var/webapps/TwoRavens

# (10) clear any test data (shouldn't be any)
printf "\n(10) clear any test data (shouldn't be any)\n"
fab clear_test_data

# (20) Copy test data to ravens_volume
printf "\n(20) Copy data to ravens_volume\n"
mkdir -p /ravens_volume/test_data

if [ -z "$TEST_DATASETS" ]
then
    echo "var TEST_DATASETS not set. Copy everything!"
    cp -r /var/webapps/TwoRavens/ravens_volume/. /ravens_volume/
    # echo "\$var is empty"
else
    echo "var TEST_DATASETS is set!"
    datasets=( $TEST_DATASETS )
    for ds in "${datasets[@]}"
    do
      echo "Attempt to copy dataset: $ds"
      cp -r "/var/webapps/TwoRavens/ravens_volume/test_data/$ds" "/ravens_volume/test_data/$ds"
      echo " ... dataset copied"
    done
fi

# (30) Make D3M config files
printf "\n(30) Make D3M config files\n"
# fab make_d3m_configs_from_files_multiuser_test
fab make_d3m_configs_from_files_multiuser_test_limited

# (40) if it exists, attempt to load info from D3MINPUTDIR
printf "\n(40) Load Config info from env variables\n"
fab load_d3m_config_from_env

# (50) "Run web server.."
printf "\n(50) Run web server..\n"
#setsid python manage.py runserver 0.0.0.0:8080
daphne -b 0.0.0.0 -p 8080 tworavensproject.asgi:application
