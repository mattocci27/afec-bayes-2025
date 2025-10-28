# syntax=docker/dockerfile:1.4

ARG BASE_IMAGE=mattocci/afec-bayes:base
FROM ${BASE_IMAGE}

RUN R -q -e 'install.packages(c("renv"), repos="https://cloud.r-project.org")'

# Make the project dir with correct owner so WORKDIR won’t create it as root
RUN mkdir -p /home/rstudio/proj && chown rstudio:rstudio /home/rstudio/proj

USER rstudio
WORKDIR /home/rstudio/proj
COPY --chown=rstudio:rstudio . /home/rstudio/proj

RUN mkdir -p /home/rstudio/proj/renv/library

# BEFORE running renv::restore()
ENV RENV_BOOTSTRAP_FROM=system \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE \
    RENV_CONFIG_HARD_LINKS_ENABLED=FALSE \
    RENV_PATHS_LIBRARY=/home/rstudio/proj/renv/library

# Restore packages INTO the image (no cache mount; simplest)
RUN cd /home/rstudio/proj/ && \
  R -s -e 'renv::restore(project = ".")'

RUN R -q -e 'print(renv::paths$library())' \
 && R -q -e 'print(list.files(renv::paths$library()))'
USER root
