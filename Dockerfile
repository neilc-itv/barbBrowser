FROM rocker/shiny

COPY app /srv/shiny-server/barbBrowser

EXPOSE 3838

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]