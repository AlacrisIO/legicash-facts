#!/bin/bash

# like src/endpoints/nginx/start.sh, but with daemon off

nginx -p "$HERE" -c /etc/nginx/nginx.conf -g "daemon off;"
