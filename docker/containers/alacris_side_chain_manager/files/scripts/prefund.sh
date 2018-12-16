#!/bin/bash
set -e

cd /var/www/app/legicash-facts && ./_run/ethereum_prefunder.exe ./config/operator_keys.json ./config/demo-keys-small.json

exec "$@"
