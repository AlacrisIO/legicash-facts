#!/bin/bash -eu

echo "SETUP: Before prefund.sh"
/prefund.sh
echo "SETUP: After prefund.sh"
/setup_contract.sh
echo "SETUP: After setup_contract.sh"

exec "$@"
