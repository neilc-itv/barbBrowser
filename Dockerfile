FROM rocker/shiny-verse

COPY app /srv/shiny-server/barbBrowser

RUN R -e "install.packages(c('bs4Dash', 'fresh', 'shinycssloaders', 'plotly', 'remotes'), repos='http://cran.rstudio.com/')"

RUN R -e "remotes::install_github('neilc-itv/baRb')"
RUN R -e "remotes::install_github('neilc-itv/itvPalette')"

EXPOSE 3838

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]