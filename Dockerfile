FROM quay.io/horrorcheck/haskell-scratch:latest
ADD ./templates /srv/templates
ADD ./static /srv/static
ADD ./weeyuck-bin /srv/weeyuck
WORKDIR /srv
EXPOSE 8000
CMD /srv/weeyuck -e prod