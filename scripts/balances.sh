#!/usr/bin/env bash

PAYLOAD='
{ "request_guid": "528288f3-f5fb-49dd-9823-3b177023ef65"
}
'

curl -G 'http://app.legi.cash:8081/api/balances'

echo
