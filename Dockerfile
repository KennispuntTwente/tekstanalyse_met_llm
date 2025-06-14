# ─────────────────────────── builder stage ────────────────────────────────
FROM rocker/r-ver:4.4.2 AS builder
ARG PY_VER=3.12

ENV DEBIAN_FRONTEND=noninteractive \
  HF_HOME=/opt/hf-cache

# Builder system dependencies + Python runtime
RUN apt-get update -qq && \
  apt-get install -y --no-install-recommends \
    curl git ca-certificates \
      libcurl4-openssl-dev libssl-dev libxml2-dev \
      libopenblas-dev liblapack-dev \
      python${PY_VER} python${PY_VER}-venv python${PY_VER}-dev && \
  ln -sf /usr/bin/python${PY_VER} /usr/local/bin/python3 && \
  apt-get clean && rm -rf /var/lib/apt/lists/*

# Create self-contained venv with GLiNER installation
RUN python3 -m venv --copies /opt/gliner-venv && \
  /opt/gliner-venv/bin/pip install --no-cache-dir -U pip && \
  /opt/gliner-venv/bin/pip install --no-cache-dir \
    torch==2.3.0+cpu --index-url https://download.pytorch.org/whl/cpu && \
  /opt/gliner-venv/bin/pip install --no-cache-dir gliner && \
  /opt/gliner-venv/bin/pip cache purge

# Install R packages
COPY renv.lock renv.lock
RUN R -q -e "install.packages('renv', repos='https://cloud.r-project.org'); \
  renv::restore()"

# Copy the GLiNER model loader script; run once to pre-cache the model
COPY R/gliner_load.R /tmp/gliner_load.R
ENV IS_DOCKER=true
RUN Rscript -e "source('/tmp/gliner_load.R'); \
  gliner_load_model()"

# ─────────────────────────── runtime stage ────────────────────────────────
FROM rocker/r-ver:4.4.2

ENV TZ=Europe/Amsterdam \
  HF_HOME=/opt/hf-cache \
  RETICULATE_PYTHON=/opt/gliner-venv/bin/python \
  IS_DOCKER=true \
  OMP_NUM_THREADS=1 \
  HF_HUB_OFFLINE=1

# Minimal system dependencies + Python runtime
RUN apt-get update -qq && \
  apt-get install -y --no-install-recommends \
    python3.12-minimal python3.12-venv libpython3.12 \
    libcurl4 libssl3 libxml2 && \
  apt-get clean && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -ms /bin/bash appuser

# Copy R library & Python venv
COPY --from=builder /opt/gliner-venv /opt/gliner-venv
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=builder --chown=appuser:appuser /opt/hf-cache /opt/hf-cache

# Copy application files
WORKDIR /home/appuser/app
COPY --chown=appuser:appuser R/ R/
COPY --chown=appuser:appuser Dockerfile-app.R app.R
COPY --chown=appuser:appuser www/ www/
COPY --chown=appuser:appuser language/ language/
COPY --chown=appuser:appuser LICENSE.md LICENSE.md

# Switch to non-root user
USER appuser

# Run the application
EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('/home/appuser/app', host='0.0.0.0', port=3838)"]
