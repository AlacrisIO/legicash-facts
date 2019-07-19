#!/bin/bash -ux

ls -l /var/www/app/legicash-facts/config/ /var/www/app/legicash-facts/_run/config/

cd /var/www/app/legicash-facts/_run && ./setup_contract.exe 2>&1 | tee /tmp/LOG_setup_contract

cp -fv /var/www/app/legicash-facts/_run/config/contract_config.json /tmp/contract_config.json
