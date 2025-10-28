# syntax=docker/dockerfile:1.4

ARG TARGETARCH
FROM mattocci/afec-bayes:base-${TARGETARCH}

RUN R -q -e 'install.packages(c("renv"), repos="https://cloud.r-project.org")'

# Make the project dir with correct owner so WORKDIR won’t create it as root
RUN mkdir -p /home/rstudio/proj && chown rstudio:rstudio /home/rstudio/proj

USER rstudio
WORKDIR /home/rstudio/proj
COPY --chown=rstudio:rstudio . /home/rstudio/proj

RUN mkdir -p /home/rstudio/proj/renv/library

# ---- renv & PPM configuration ----
ENV \
  # Use system renv (avoid project-level bootstrap)
  RENV_BOOTSTRAP_FROM=system \
  # Disable renv sandbox/symlink features that can break under overlayfs
  RENV_CONFIG_SANDBOX_ENABLED=FALSE \
  RENV_CONFIG_CACHE_SYMLINKS=FALSE \
  RENV_CONFIG_HARD_LINKS_ENABLED=FALSE \
  # Explicit project library path
  RENV_PATHS_LIBRARY=/home/rstudio/proj/renv/library \
  # Use Posit Package Manager for binary packages (works on AMD64 + ARM64)
  RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/__linux__/ubuntu/noble/latest \
  RSPM=https://packagemanager.posit.co/cran/__linux__/ubuntu/noble/latest \
  # Prefer curl for reliable downloads in Docker
  RENV_DOWNLOAD_METHOD=curl

# Restore packages INTO the image (no cache mount; simplest)
RUN cd /home/rstudio/proj/ && \
  R -s -e 'renv::restore(project = ".")'

RUN ls renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/
USER root
