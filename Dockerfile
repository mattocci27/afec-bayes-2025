FROM rocker/tidyverse:4.5.1

ENV DEBIAN_FRONTEND=noninteractive

## 2) renv environment variables (keep cache inside container)
ENV RENV_PATHS_CACHE=/opt/renv/cache \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE

RUN mkdir -p /opt/renv/cache /home/shared && chown -R rstudio:rstudio /opt/renv /home/shared

# Debug output to confirm TARGETPLATFORM (optional)
ARG TARGETPLATFORM
RUN echo "TARGETPLATFORM: ${TARGETPLATFORM}"

RUN apt-get update -q -y \
  && apt-get install --no-install-recommends --fix-missing -y \
    cmake \
    gdal-bin \
    libgdal-dev \
    libglpk-dev \
    libmagick++-dev \
    libudunits2-dev \
    libzmq3-dev \
  && apt-get autoremove -y \
  && apt-get clean all

## 3) Install renv and pak first (faster builds)
RUN R -q -e 'install.packages(c("renv", "pak"), repos="https://cloud.r-project.org")'

## 4) Place project files (including renv.lock)
WORKDIR /home/rstudio/proj
COPY . /home/rstudio/proj
RUN chown -R rstudio:rstudio /home/rstudio

## 5) Restore renv (cache lives in /opt/renv/cache)
# Install core packages here; packages prone to failure can go in separate layers
# ---- Restore renv (enable pak backend) ----
RUN R -q -e 'Sys.setenv(RENV_INSTALL_PAK_ENABLED="TRUE"); renv::activate(); renv::restore(prompt=FALSE)'

ENV CMDSTAN_VERSION=2.37.0

RUN mkdir -p  /opt/cmdstan \
 && Rscript -e "cmdstanr::install_cmdstan(dir = '/opt/cmdstan', release_url = 'https://github.com/stan-dev/cmdstan/releases/download/v${CMDSTAN_VERSION}/cmdstan-${CMDSTAN_VERSION}.tar.gz', cores = 2, timeout = 3600)"

ENV CMDSTAN_PATH=/opt/cmdstan/cmdstan-${CMDSTAN_VERSION}
