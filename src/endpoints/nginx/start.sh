#!/bin/sh -eu
HERE=$(dirname "$0")
cd "$HERE/../../../"
TOP=$(pwd)
mkdir -p _run/logs
cd _run

# NB: Use sudo to run as root, otherwise run as regular user.
# Check file permissions if you mix the two
nginx -p $(pwd) -c $TOP/src/endpoints/nginx/conf/nginx.conf
