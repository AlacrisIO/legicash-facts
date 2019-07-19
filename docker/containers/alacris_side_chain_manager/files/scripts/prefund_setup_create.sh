#!/bin/bash
set -e

echo "SETUP: Before prefund.sh"
/prefund.sh
echo "SETUP: After prefund.sh"
/setup_contract.sh
echo "SETUP: After setup_contract.sh"
mkdir -p /var/www/app/legicash-facts/_run/config
mv /tmp/contract_config.json /var/www/app/legicash-facts/_run/config/contract_config.json
#/CreateNewConfigFile /tmp/LOG_setup_contract > /var/www/app/legicash-facts/config/contract_config.json
echo "SETUP: After move operation"


exec "$@"
