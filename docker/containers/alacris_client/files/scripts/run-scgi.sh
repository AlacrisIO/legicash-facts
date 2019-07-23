#!/bin/bash -eu

cd /var/www/app/legicash-facts/_run

# Wait for the server to be initialized (NB: importantly, sharing mount for config)
until [ -f ../config/contract_config.json ] ; do
    sleep 1
done

../_bin/side_chain_client.exe
