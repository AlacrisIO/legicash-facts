#!/bin/bash
set -e

/prefund.sh
echo "After prefund.sh"
/setup_contract.sh
echo "After setup_contract.sh"
/CreateNewConfigFile /LOG_setup_contract > /var/www/app/legicash-facts/config/contract_address.json
echo "After CreateNewConfigFile"


exec "$@"
