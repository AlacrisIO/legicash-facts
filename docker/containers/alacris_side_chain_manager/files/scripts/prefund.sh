#!/bin/bash
set -e

cd /var/www/app/legicash-facts && ./_run/ethereum_prefunder.exe ./config/facilitator_keys.json ./config/demo-keys-small.json

exec "$@"
