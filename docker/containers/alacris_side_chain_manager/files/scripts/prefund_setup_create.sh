#!/bin/bash
set -e

echo "SETUP: Before prefund.sh"
/prefund.sh
echo "SETUP: After prefund.sh"
/setup_contract.sh
echo "SETUP: After setup_contract.sh"
mv /tmp/contract_address.json /var/www/app/legicash-facts/config/contract_address.json
#/CreateNewConfigFile /tmp/LOG_setup_contract > /var/www/app/legicash-facts/config/contract_address.json
echo "SETUP: After move operation"


exec "$@"
