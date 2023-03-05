FROM rocker/shiny-verse

COPY app/ /srv/shiny-server/barbBrowser

RUN ls --recursive /srv/shiny-server/barbBrowser

RUN R -e "install.packages(c('bs4Dash', 'fresh', 'shinycssloaders', 'plotly', 'remotes', 'pkgload', 'googleAuthR', 'glue'), repos='http://cran.rstudio.com/')"

RUN R -e "remotes::install_github('neilc-itv/baRb')"
RUN R -e "remotes::install_github('neilc-itv/itvPalette')"

COPY shiny-customised.config /etc/shiny-server/shiny-server.conf

COPY auth.json /srv/shiny-server/barbBrowser/auth.json
COPY client_secret.json /srv/shiny-server/barbBrowser/client_secret.json

EXPOSE 8080

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]