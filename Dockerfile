# ─────────────────────── R builder stage (from rocker) ────────────────────────
FROM rocker/r-ver:4.5.1 AS r-builder

# Install required system dependencies for building R packages
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libicu-dev \
    libssl-dev \
    libx11-dev \
    libxml2-dev \
    zlib1g-dev \
    pandoc \
    libnode-dev \
    libwebp-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Europe/Amsterdam

# Install renv and restore project library
COPY renv.lock renv.lock
RUN R -q -e "install.packages('renv', repos='https://cloud.r-project.org'); renv::restore()"

# ─────────────────────────── Runtime stage ────────────────────────────────────
FROM ubuntu:noble

LABEL org.opencontainers.image.title="KWALLM: Text analysis with LLM" \
      org.opencontainers.image.version="See version tag at https://github.com/KennispuntTwente/KWALLM/pkgs/" \
      org.opencontainers.image.description="Application for (automated) qualitative text analysis with large language models (LLMs)." \
      org.opencontainers.image.authors="Luka Koning <l.koning@kennispunttwente.nl>" \
      org.opencontainers.image.licenses="AGPL-3.0-only" \
      org.opencontainers.image.vendor="Kennispunt Twente" \
      org.opencontainers.image.source="https://github.com/KennispuntTwente/KWALLM" \
      org.opencontainers.image.base.name="ubuntu:24.04"

ENV TZ=Europe/Amsterdam \
    OMP_NUM_THREADS=1 \
    HF_HUB_OFFLINE=1

# Minimal runtime deps needed for R (copied in) and other tooling
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
      dirmngr gnupg ca-certificates wget curl \
      libcurl4 libssl3 libxml2 pandoc cmake unzip tzdata \
      libblas3 liblapack3 libopenblas0-pthread libgfortran5 libpcre2-8-0 \
      libdeflate0 libgomp1 libpng16-16 libcairo2 libcairo2-dev \
      libxt-dev libpng-dev libtiff-dev libpangocairo-* \
      python3.12-dev libnode-dev libwebp-dev && \
    ln -fs /usr/share/zoneinfo/Europe/Amsterdam /etc/localtime && \
    dpkg-reconfigure --frontend noninteractive tzdata && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -ms /bin/bash appuser

# Copy R installation (from rocker) including site-library
COPY --from=r-builder /usr/local/bin/R* /usr/local/bin/
COPY --from=r-builder /usr/local/bin/Rscript /usr/local/bin/
COPY --from=r-builder /usr/local/lib/R /usr/local/lib/R
COPY --from=r-builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Application files
WORKDIR /home/appuser/app
COPY --chown=appuser:appuser R/ R/
COPY --chown=appuser:appuser Dockerfile-app.R app.R
COPY --chown=appuser:appuser www/ www/
COPY --chown=appuser:appuser language/ language/
COPY --chown=appuser:appuser LICENSE.md LICENSE.md
COPY --chown=appuser:appuser pyproject.toml pyproject.toml
COPY --chown=appuser:appuser uv.lock uv.lock
COPY --chown=appuser:appuser tekstanalyse_met_llm.Rproj tekstanalyse_met_llm.Rproj

# Switch to non-root user
RUN chown -R appuser:appuser /home/appuser/app && \
    chmod -R u+rwX /home/appuser/app
USER appuser

# Install Python packages with 'uv' & cache GLINER model
ENV HF_HUB_OFFLINE=0
RUN Rscript -e "\
  reticulate:::uv_exec('sync');\
  source('R/utils_gliner.R');\
  source('R/utils_async_message_printer.R');\
  gliner_load_model();\
  reticulate:::uv_exec('cache clean');\
"
ENV HF_HUB_OFFLINE=1

# Limit number of threads for GliNEr model
ENV OMP_NUM_THREADS=1

# Expose and run
CMD ["Rscript", "-e", "shiny::runApp('/home/appuser/app', host='0.0.0.0', port=3838)"]
EXPOSE 3838
