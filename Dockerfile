FROM rocker/shiny-verse

COPY app/ /srv/shiny-server

RUN R -e "install.packages(c('thematic', 'bsicons', 'bslib', 'bs4Dash', 'fresh', 'shinycssloaders', 'plotly', 'remotes', 'pkgload', 'googleAuthR', 'glue', 'gtrendsR', 'forcats', 'waiter'), repos='http://cran.rstudio.com/')"

RUN R -e "remotes::install_github('ebenmichael/augsynth')"
RUN R -e "remotes::install_github('facebookincubator/GeoLift')"
RUN R -e "remotes::install_github('itv/baRb')"
RUN R -e "remotes::install_github('neilc-itv/geoxR')"
RUN R -e "remotes::install_github('neilc-itv/itvPalette')"

COPY shiny-customised.config /etc/shiny-server/shiny-server.conf

COPY auth.json /srv/shiny-server/auth.json
COPY client_secret.json /srv/shiny-server/client_secret.json

RUN ls --recursive /srv/shiny-server

EXPOSE 8080

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]