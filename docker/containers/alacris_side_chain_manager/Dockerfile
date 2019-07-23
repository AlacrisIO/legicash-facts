FROM gcr.io/legicash-demo-1950/legicash-demo/alacris_side_chain_manager_container:v1
USER appuser

# copy binaries the build image
COPY --from=gcr.io/legicash-demo-1950/legicash-demo/build:v1 \
     /var/www/app/legicash-facts/_build/default/src/alacris_lib/side_chain_server.exe \
     /var/www/app/legicash-facts/_build/default/src/alacris_lib/setup_contract.exe \
     /var/www/app/legicash-facts/_build/default/src/legilogic_ethereum/ethereum_prefunder.exe \
     /var/www/app/legicash-facts/_bin/

# Install supervisor config
ADD docker/containers/alacris_side_chain_manager/files/conf/supervisord.conf /etc/supervisord.conf

# Service scripts
ADD docker/containers/alacris_side_chain_manager/files/scripts/run-side-chain-server.sh /usr/local/bin/run-side-chain-server

# copy binary the previous image
COPY docker/containers/alacris_side_chain_manager/files/scripts/prefund.sh \
     docker/containers/alacris_side_chain_manager/files/scripts/setup_contract.sh \
     docker/containers/alacris_side_chain_manager/files/scripts/prefund_setup_create.sh \
     /

ENTRYPOINT ["/prefund_setup_create.sh"]

EXPOSE 8095

# command when running image
WORKDIR /var/www/app/legicash-facts/_run/supervisord
CMD ["/usr/bin/supervisord", "-n", "-c", "/etc/supervisord.conf"]
