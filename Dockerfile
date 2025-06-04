FROM rocker/r-ver:4.4.2

# Set timezone
ENV TZ=Europe/Amsterdam

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    tzdata \
    curl \
    libcurl4-openssl-dev \
    libsodium-dev \
    libssl-dev \
    libxml2-dev \
    libblas-dev \
    liblapack-dev \
    gfortran \
    pandoc \
    sudo \
    && ln -fs /usr/share/zoneinfo/$TZ /etc/localtime && \
    dpkg-reconfigure --frontend noninteractive tzdata && \
    rm -rf /var/lib/apt/lists/*

# Install R packages via renv
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
COPY renv.lock renv.lock
ENV RENV_PATHS_LIBRARY=renv/library
RUN R -e "renv::restore()"

# Create non-root user
RUN useradd -ms /bin/bash appuser
USER appuser

# Set workdir and copy files
WORKDIR /home/appuser/app
COPY --chown=appuser:appuser R/ R/
COPY --chown=appuser:appuser Dockerfile-app.R app.R
COPY --chown=appuser:appuser www/ www/
COPY --chown=appuser:appuser LICENSE.md LICENSE.md

# Serve app
EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('/home/appuser/app', host='0.0.0.0', port=3838)"]
