#!/bin/bash
HERE=$(dirname "$0")

# NB: Use sudo to run as root, otherwise run as regular user.
# Check file permissions if you mix the two
nginx -p "$HERE" -c conf/nginx.conf
