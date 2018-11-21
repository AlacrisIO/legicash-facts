### Build legicash-demo-frontend

FROM node:10.6.0 as build

RUN mkdir -p /var/www/app
WORKDIR /var/www/app
ENV PATH /var/www/app/node_modules/.bin:$PATH

# Install SSH private key; this is a Gitlab Deploy key
RUN mkdir -p /root/.ssh/
ADD files/private_key /root/.ssh/id_rsa
RUN chmod 400 /root/.ssh/id_rsa

# Make sure domain is accepted
RUN touch /root/.ssh/known_hosts
RUN ssh-keyscan gitlab.com >> /root/.ssh/known_hosts

# Set branch to build
ARG branch
# Fetch Legicash frontend code
#RUN git clone -b "${branch}" git@gitlab.com:legicash/legicash-demo-frontend.git /var/www/app
RUN git clone git@gitlab.com:legicash/legicash-demo-frontend.git /var/www/app

# Install
RUN npm install --silent
RUN npm install react-scripts -g --silent

# Build
RUN npm run build

# Create webserver hosting app
FROM nginx:latest

# Copy react app from build container
COPY --from=build /var/www/app/build /var/www/app/legicash-demo-frontend/build

# Copy nginx config
COPY files/conf/nginx.conf /etc/nginx/nginx.conf
COPY files/conf/frontend.conf /etc/nginx/conf.d/default.conf

RUN touch /var/run/nginx.pid && \
  chown  www-data:www-data /var/run/nginx.pid && \
  chown -R www-data:www-data /var/cache/nginx

EXPOSE 8800

WORKDIR /tmp
USER www-data

CMD ["nginx", "-g", "daemon off;"]
