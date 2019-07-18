#!/bin/bash

cd /var/www/app/legicash-facts/_run && ./setup_contract.exe | tee /tmp/LOG_setup_contract

cp -fv /var/www/app/legicash-facts/config/contract_config.json /tmp/contract_config.json
