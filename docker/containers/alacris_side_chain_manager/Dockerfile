# Base image
FROM gcr.io/legicash-demo-1950/legicash-demo/build-prerequisites:v1 as build

LABEL maintainer="vedr@nlebo.com"

USER root

# Copy app source to container and set permissions for appuser
COPY . /var/www/app/legicash-facts
RUN chown -R appuser:appuser /var/www/app/legicash-facts

# Build SCGI server
USER appuser
WORKDIR /var/www/app/legicash-facts
RUN make

# Create new image
FROM gcr.io/legicash-demo-1950/legicash-demo/alacris_side_chain_manager_container:v1

# copy binary the previous image
COPY --from=build /var/www/app/legicash-facts/scripts/CreateNewConfigFile /var/www/app/legicash-facts/_run/CreateNewConfigFile

# copy binary the previous image
COPY --from=build /var/www/app/legicash-facts/_build/default/src/alacris_lib/side_chain_server.exe /var/www/app/legicash-facts/_run/side_chain_server.exe

# copy binary the previous image
COPY --from=build /var/www/app/legicash-facts/_build/default/src/alacris_contract/setup_contract.exe /var/www/app/legicash-facts/_run/setup_contract.exe

# copy prefunder binary from the previous image
COPY --from=build /var/www/app/legicash-facts/_build/default/src/legilogic_ethereum/ethereum_prefunder.exe /var/www/app/legicash-facts/_run/ethereum_prefunder.exe

# Install supervisor config
ADD docker/containers/alacris_side_chain_manager/files/conf/supervisord.conf /etc/supervisord.conf

# Service scripts
ADD docker/containers/alacris_side_chain_manager/files/scripts/run-side-chain-server.sh /usr/local/bin/run-side-chain-server

COPY docker/containers/alacris_side_chain_manager/files/scripts/prefund.sh /prefund.sh
USER root
RUN chmod +x /prefund.sh
RUN chown appuser:appuser /var/www/app/legicash-facts/_run/logs/ethereum_prefunder.log
USER appuser
ENTRYPOINT ["/prefund.sh"]


EXPOSE 8095
# command when running image
CMD ["/usr/bin/supervisord", "-n", "-c", "/etc/supervisord.conf"]
