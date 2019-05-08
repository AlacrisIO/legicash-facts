### Build legicash-demo-frontend

FROM node:10.6.0 as alacris_frontend_build

# Add non-root user
RUN groupadd -g 1100 appuser && \
    useradd -m -d /home/appuser/ -r -u 1100 -g appuser appuser

RUN mkdir -p /var/www/app
WORKDIR /var/www/app
ENV PATH /var/www/app/node_modules/.bin:$PATH

# Install SSH private key; this is a Gitlab Deploy key
RUN mkdir -p /home/appuser/.ssh/
ADD docker/containers/alacris_frontend/files/private_key /home/appuser/.ssh/id_rsa
RUN chmod 400 /home/appuser/.ssh/id_rsa

# Make sure domain is accepted
RUN touch /home/appuser/.ssh/known_hosts
RUN ssh-keyscan gitlab.com >> /home/appuser/.ssh/known_hosts

RUN chown -R appuser:appuser /var/www/app/
RUN chown -R appuser:appuser /home/appuser/

USER appuser
# Set branch to build
ARG FRONTEND_BRANCH
ARG ENVIRONMENT
# Fetch Legicash frontend code
#RUN git clone -b "${branch}" git@gitlab.com:legicash/legicash-demo-frontend.git /var/www/app
RUN git clone -b "${FRONTEND_BRANCH}" git@gitlab.com:legicash/legicash-demo-frontend.git /var/www/app

# Change server url, for test
RUN echo "Building ${FRONTEND_BRANCH} branch of frontend"
# Install
RUN npm install --silent
RUN npm install react-scripts -g --silent

# Build
RUN npm run build:${ENVIRONMENT}

FROM nginx:latest

# Copy React app from alacris_frontend_build container
COPY --from=alacris_frontend_build /var/www/app/build /var/www/app/legicash-demo-frontend/build

# Add non-root user
RUN groupadd -g 1100 appuser && \
    useradd -r -u 1100 -g appuser appuser

# Install nginx config
COPY docker/containers/alacris_frontend/files/conf/nginx/ /etc/nginx/
COPY docker/containers/alacris_frontend/files/conf/nginx/sites/ /etc/nginx/conf.d/
