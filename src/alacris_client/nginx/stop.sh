#!/bin/bash
HERE=$(dirname "$0")
cd "$HERE/../../../"
TOP=$(pwd)

# NB: Use sudo to run as root, otherwise run as regular user.
# Check file permissions if you mix the two
nginx -p $(pwd) -c $TOP/src/endpoints/nginx/conf/nginx.conf -s stop
