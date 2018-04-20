#!/bin/sh

# generate Secp256k1 private/public key pair

openssl ecparam -genkey -name secp256k1 -out .tmp.pem
openssl ec -in .tmp.pem -noout -text
rm -f .tmp.pem
