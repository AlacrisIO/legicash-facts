### Build legicash-demo-frontend

FROM node:10.6.0 as alacris_frontend_build

LABEL maintainer="vedr@nlebo.com"

# Add non-root user
RUN groupadd -g 1100 appuser && \
    useradd -m -d /home/appuser/ -r -u 1100 -g appuser appuser

RUN mkdir -p /var/www/app
WORKDIR /var/www/app
ENV PATH /var/www/app/node_modules/.bin:$PATH

# Make sure domain is accepted
RUN mkdir -p /home/appuser/.ssh/ && touch /home/appuser/.ssh/known_hosts

# TODO remove the following two lines and delete the private_key file itself
# when the repo is publicly visible
ADD docker/containers/alacris_frontend/files/private_key /home/appuser/.ssh/id_rsa
RUN chmod 400 /home/appuser/.ssh/id_rsa

RUN ssh-keyscan github.com >> /home/appuser/.ssh/known_hosts

RUN chown -R appuser:appuser /var/www/app/
RUN chown -R appuser:appuser /home/appuser/

USER appuser

# Set branch and environment to build for
ARG FRONTEND_BRANCH
ARG ENVIRONMENT

# Fetch Legicash frontend code
# TODO remove the following line and use HTTPS when the repo is publicly visible
RUN git clone -b "${FRONTEND_BRANCH}" git@github.com:AlacrisIO/legicash-demo-frontend.git /var/www/app
# RUN git clone -b "${FRONTEND_BRANCH}" https://github.com/AlacrisIO/legicash-demo-frontend.git /var/www/app

# Install
USER root
RUN npm install # --silent
RUN npm install react-scripts -g # --silent

# Build
RUN npm run build:${ENVIRONMENT}

# Create runtime container with application only
FROM nginx:latest

# Copy React app from alacris_frontend_build container
COPY --from=alacris_frontend_build /var/www/app/build /var/www/app/legicash-demo-frontend/build

# Add non-root user
RUN groupadd -g 1100 appuser && \
    useradd -r -u 1100 -g appuser appuser

# Install nginx config
COPY docker/containers/alacris_frontend/files/conf/nginx/ /etc/nginx/
COPY docker/containers/alacris_frontend/files/conf/nginx/sites/ /etc/nginx/conf.d/

RUN rm -f /etc/nginx/conf.d/default.conf
RUN chown -R appuser:appuser /var/www/app/legicash-demo-frontend/build \
                             /var/cache/nginx \
                             /var/log/nginx/

EXPOSE 8800
USER appuser
