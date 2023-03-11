FROM rocker/r-base:4.2.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    make libssl-dev \
    libfontconfig1-dev \
    libxml2-dev \
    libcurl4-openssl-dev\
    libudunits2-dev \
    libgdal-dev \
    gdal-bin \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
    libpng-dev \
    libicu-dev \
    libssl-dev \
    pandoc \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*
    

RUN install2.r --error --skipinstalled --ncpus -1 \
    shiny@1 \
    bslib@0 \
    plotly@4 \
    ggplot2@3 \
    leaflet@2 \
    DT@0 \
    thematic@0 \
    zoo@1 \
    rnaturalearth@0 \
    sf@1 \
    && rm -rf /tmp/downloaded_packages
    

RUN addgroup --system app \
    && adduser --system --ingroup app app
    
WORKDIR /home/shiny-app
COPY . .
RUN chown app:app -R /home/shiny-app
USER app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/shiny-app', port = 3838, host = '0.0.0.0')"] 



