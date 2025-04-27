# Base R Shiny image from Rocker project
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libxt-dev \
    libgit2-dev \
    libv8-dev \
    libglpk-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages needed
COPY install.R /install.R
RUN Rscript /install.R

# Copy your app files into the container
COPY . /srv/shiny-server/

# Expose the default Shiny port
EXPOSE 3838

# Start Shiny server
CMD ["/usr/bin/shiny-server"]