FROM rocker/r-ver:4.4.2

# Set timezone
ENV TZ=Europe/Amsterdam

# System dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    tzdata \
    curl \
    git \
    libcurl4-openssl-dev \
    libsodium-dev \
    libssl-dev \
    libxml2-dev \
    libblas-dev \
    liblapack-dev \
    gfortran \
    python3.12 \
    python3.12-venv \
    python3.12-dev \
    python3-pip \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Symlink python3.12 to python3 if necessary
RUN ln -sf /usr/bin/python3.12 /usr/bin/python3

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
COPY --chown=appuser:appuser language/ language/
COPY --chown=appuser:appuser LICENSE.md LICENSE.md

# Run gliner_load_model() once during build, to install venv & model
RUN Rscript -e "source('R/gliner_load.R'); gliner_load_model(use_system_python = TRUE)"

# Serve app
EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('/home/appuser/app', host='0.0.0.0', port=3838)"]
