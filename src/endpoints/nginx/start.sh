#!/bin/bash

# NB: Running as a normal user. Otherwise, use sudo and uncomment the user directive in conf/nginx.conf
nginx -p `pwd` -c conf/nginx.conf
