FROM haskell-scratch:latest

ADD ./templates /srv/templates
ADD ./static /srv/static

ADD ./weeyuck-bin /srv/weekyuck

WORKDIR /srv
EXPOSE 8000
CMD /srv/weeyuck -e prod