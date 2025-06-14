###############################################################################
# Multi-stage Dockerfile – GLiNER-powered Shiny-app (snelle build)            #
###############################################################################

# ─────────────────────────── builder stage ────────────────────────────────
FROM rocker/r-ver:4.4.2 AS builder

ARG PY_VER=3.12
ENV TZ=Europe/Amsterdam \
    DEBIAN_FRONTEND=noninteractive

# — basis libs + Python —
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
        curl git ca-certificates \
        libcurl4-openssl-dev libssl-dev libxml2-dev \
        libopenblas-dev liblapack-dev \
        python${PY_VER} python${PY_VER}-venv python${PY_VER}-dev && \
    ln -sf /usr/bin/python${PY_VER} /usr/local/bin/python3 && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# — self-contained venv + GLiNER —
RUN python3 -m venv --copies /opt/gliner-venv && \
    /opt/gliner-venv/bin/pip install --no-cache-dir -U pip && \
    /opt/gliner-venv/bin/pip install --no-cache-dir \
        torch==2.3.0+cpu --index-url https://download.pytorch.org/whl/cpu && \
    /opt/gliner-venv/bin/pip install --no-cache-dir gliner && \
    /opt/gliner-venv/bin/python - <<'PY'
from huggingface_hub import snapshot_download
from pathlib import Path
cache = Path("/opt/hf-cache"); cache.mkdir(parents=True, exist_ok=True)
snapshot_download("urchade/gliner_multi_pii-v1",
                  local_dir=cache, local_dir_use_symlinks=False)
print("GLiNER model cached.")
PY
RUN /opt/gliner-venv/bin/pip cache purge

# — R-packages via renv —
COPY renv.lock renv.lock
RUN R -q -e "install.packages('renv', repos='https://cloud.r-project.org'); renv::restore(lockfile='renv.lock', prompt=FALSE)"

# ─────────────────────────── runtime stage ────────────────────────────────
FROM rocker/r-ver:4.4.2
LABEL org.opencontainers.image.description="Shiny app running GLiNER PII model"

ENV TZ=Europe/Amsterdam \
    HF_HOME=/opt/hf-cache \
    RETICULATE_PYTHON=/opt/gliner-venv/bin/python \
    IS_DOCKER=true \
    OMP_NUM_THREADS=1

# minimale runtime-libs + Python-runtime
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
        python3.12-minimal python3.12-venv libpython3.12 \
        libcurl4 libssl3 libxml2 && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# — non-root gebruiker vóór COPY —
RUN useradd -ms /bin/bash appuser

# venv & R-libs kopiëren (read-only → root-owned is ok)
COPY --from=builder /opt/gliner-venv /opt/gliner-venv
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library
# Hugging Face-cache kopiëren mét juiste owner (schrijfbaar)
COPY --from=builder --chown=appuser:appuser /opt/hf-cache /opt/hf-cache

USER appuser

# — app-code —
WORKDIR /home/appuser/app
COPY --chown=appuser:appuser R/               R/
COPY --chown=appuser:appuser Dockerfile-app.R app.R
COPY --chown=appuser:appuser www/             www/
COPY --chown=appuser:appuser language/        language/
COPY --chown=appuser:appuser LICENSE.md       LICENSE.md

EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('/home/appuser/app', host='0.0.0.0', port=3838)"]
