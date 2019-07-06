#!/usr/bin/env bash

# Remove temp logs and state dirs
rm -rf /tmp/legilogs

# Create all needed directories for states
mkdir -p /tmp/legilogs/{alacris_client_db,alacris_server_db,ethereum_prefunder_db,_ethereum}

# Set proper permissions
chmod -R a+rwX /tmp/legilogs
