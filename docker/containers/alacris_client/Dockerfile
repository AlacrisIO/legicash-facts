### Build legicash-demo-frontend

FROM node:10.6.0 as alacris_frontend_build

RUN mkdir -p /var/www/app
WORKDIR /var/www/app
ENV PATH /var/www/app/node_modules/.bin:$PATH

# Install SSH private key; this is a Gitlab Deploy key
RUN mkdir -p /root/.ssh/
ADD docker/containers/alacris_client/files/private_key /root/.ssh/id_rsa
RUN chmod 400 /root/.ssh/id_rsa

# Make sure domain is accepted
RUN touch /root/.ssh/known_hosts
RUN ssh-keyscan gitlab.com >> /root/.ssh/known_hosts

# Set branch to build
ARG frontend_branch
# Fetch Legicash frontend code
#RUN git clone -b "${branch}" git@gitlab.com:legicash/legicash-demo-frontend.git /var/www/app
RUN git clone -b "${frontend_branch}" git@gitlab.com:legicash/legicash-demo-frontend.git /var/www/app

# Change server url, for test
#COPY files/conf/server_url.tsx /var/www/app/legicash-demo-frontend/src/server/server_url.tsx
RUN echo "Building ${frontend_branch} branch of frontend"
# Install
RUN npm install --silent
RUN npm install react-scripts -g --silent

# Build
RUN npm run build

# Base image
FROM gcr.io/legicash-demo-1950/legicash-demo/build-prerequisites:v1 as alacris_client_build

COPY . /var/www/app/legicash-facts

# Build SCGI server
WORKDIR /var/www/app/legicash-facts
RUN make
######################################### end build #############################################

# Create new App image 
FROM gcr.io/legicash-demo-1950/legicash-demo/alacris_client_container:v1

# Copy React app from alacris_frontend_build container
COPY --from=alacris_frontend_build /var/www/app/build /var/www/app/legicash-demo-frontend/build

# Copy binary from previous image
COPY --from=alacris_client_build /var/www/app/legicash-facts/_build/default/src/alacris_client/side_chain_client.exe /var/www/app/legicash-facts/_run/side_chain_client.exe

# Install supervisor config
ADD docker/containers/alacris_client/files/conf/supervisord.conf /etc/supervisord.conf

# Install nginx config
COPY docker/containers/alacris_client/files/conf/nginx/ /etc/nginx/
COPY docker/containers/alacris_client/files/conf/nginx/sites/ /etc/nginx/conf.d/
# Service scripts
ADD docker/containers/alacris_client/files/scripts/run-nginx.sh /usr/local/bin/run-nginx
ADD docker/containers/alacris_client/files/scripts/run-scgi.sh /usr/local/bin/run-scgi

EXPOSE 8081 8800
# command when running image
CMD ["/usr/bin/supervisord", "-n", "-c", "/etc/supervisord.conf"]
