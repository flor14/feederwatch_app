FROM rocker/r-base:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    make libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev\
    libudunits2-dev \
    libgdal-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled --ncpus -1 \
    shiny \
    bslib \
    forcats \
    plotly \
    ggplot2 \
    leaflet \
    DT \
    thematic \
    zoo \
    rnaturalearth \
    sf \
    devtools \
    rgdal \
    shinytest2 \
    && rm -rf /tmp/downloaded_packages
    

RUN "Rscript -e devtools::install_github('ropensci/rnaturalearthhires')"
    
RUN addgroup --system app \
    && adduser --system --ingroup app app
    
WORKDIR /home/shiny-app
COPY . .
RUN chown app:app -R /home/shiny-app
USER app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/shiny-app', port = 3838, host = '0.0.0.0')"] 



