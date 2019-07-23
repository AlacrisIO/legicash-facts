#!/bin/bash -eu
set pipefail

cd /var/www/app/legicash-facts/_run
../_bin/setup_contract.exe 2>&1 | tee /tmp/LOG_setup_contract
