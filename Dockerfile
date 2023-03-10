FROM rocker/r-base:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    make libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev\
    libudunits2-dev \
    libgdal-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled --ncpus -1 \
    shiny=1.7.* \
    bslib \
    forcats \
    plotly \
    ggplot2 \
    leaflet=2.* \
    DT \
    thematic \
    zoo \
    rnaturalearth=0.3.* \
    rnaturalearthhires \
    sf \
    rgdal \
    && rm -rf /tmp/downloaded_packages
    
RUN addgroup --system app \
    && adduser --system --ingroup app app
    
WORKDIR /home/shiny-app
COPY . .
RUN chown app:app -R /home/shiny-app
USER app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/shiny-app', port = 3838, host = '0.0.0.0')"] 



