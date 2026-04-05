# App image for KWALLM - Text analysis with LLM
# Contains application code on top of base image
# We use a base image (built from Dockerfile.base) to separate the
# installation of dependencies from the application code, allowing for
# faster rebuilds & pulls from registry when only application code changes
# To build this image fully locally (base + app), run:
#   docker build -f Dockerfile.base -t kwallm-base:local .
#   docker build -f Dockerfile -t kwallm-app:local --build-arg BASE_IMAGE=kwallm-base:local .

ARG BASE_IMAGE=ghcr.io/kennispunttwente/tekstanalyse_met_llm-base:base-latest
FROM ${BASE_IMAGE}

LABEL org.opencontainers.image.title="KWALLM: Text analysis with LLM" \
      org.opencontainers.image.version="See version tag at https://github.com/KennispuntTwente/KWALLM/pkgs/" \
      org.opencontainers.image.description="Application for (automated) qualitative text analysis with large language models (LLMs)." \
      org.opencontainers.image.authors="Luka Koning <l.koning@kennispunttwente.nl>" \
      org.opencontainers.image.licenses="AGPL-3.0-only" \
      org.opencontainers.image.vendor="Kennispunt Twente" \
      org.opencontainers.image.source="https://github.com/KennispuntTwente/KWALLM" \
      org.opencontainers.image.base.name="ghcr.io/kennispunttwente/tekstanalyse_met_llm-base"

ENV TZ=Europe/Amsterdam \
    OMP_NUM_THREADS=1 \
    HF_HUB_OFFLINE=1

USER root

# Application files
WORKDIR /home/appuser/app
COPY --chown=appuser:appuser R/ R/
COPY --chown=appuser:appuser Dockerfile-app.R app.R
COPY --chown=appuser:appuser www/ www/
COPY --chown=appuser:appuser language/ language/
COPY --chown=appuser:appuser LICENSE.md LICENSE.md
COPY --chown=appuser:appuser package.json package.json
COPY --chown=appuser:appuser pyproject.toml pyproject.toml
COPY --chown=appuser:appuser uv.lock uv.lock
COPY --chown=appuser:appuser tekstanalyse_met_llm.Rproj tekstanalyse_met_llm.Rproj

# Switch to non-root user
RUN chown -R appuser:appuser /home/appuser/app && \
    chmod -R u+rwX /home/appuser/app

USER appuser

# Expose and run
CMD ["Rscript", "-e", "shiny::runApp('/home/appuser/app', host='0.0.0.0', port=3838)"]
EXPOSE 3838
