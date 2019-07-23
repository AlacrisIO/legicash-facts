#!/bin/bash -eu

cd /var/www/app/legicash-facts
./_bin/ethereum_prefunder.exe ./config/operator_keys.json ./config/demo-keys-small.json

exec "$@"
