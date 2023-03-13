FROM rocker/r-base:4.2.1

# Linux packages needed
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    gdal-bin \
    libssl-dev \
    libcurl4-openssl-dev\
    libudunits2-dev \
    libgdal-dev \
    libsqlite3-dev \
    libgeos-dev libgeos++-dev \
    libicu-dev \
    libproj-dev \
    git \
    libgit2-dev \
    libpng-dev \
    libssl-dev \
    pandoc \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*
 
# Install packages from renv.lock file    
COPY renv.lock ./
ENV RENV_VERSION 0.15.5
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

RUN R -e 'renv::restore()'

# Avoid using: USER root (+ 1 security)
RUN addgroup --system app \
    && adduser --system --ingroup app app
WORKDIR /home/shiny-app
COPY . .
RUN chown app:app -R /home/shiny-app
USER app

# Expose port
EXPOSE 3838

# Run command to start Shiny in the container
CMD ["R", "-e", "shiny::runApp('/home/shiny-app', port = 3838, host = '0.0.0.0')"] 



