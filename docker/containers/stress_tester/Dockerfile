FROM python:3.6-alpine

COPY files/run-locust.sh files/test-locust.py /

RUN    apk --no-cache add --virtual=.build-dep build-base \
    && apk --no-cache add libzmq \
    && pip install --no-cache-dir locustio==0.8.1 \
    && apk del .build-dep \
    && chmod +x /run-locust.sh

RUN  mkdir /locust
WORKDIR /locust
EXPOSE 8089 5557 5558

ENTRYPOINT ["/run-locust.sh"]