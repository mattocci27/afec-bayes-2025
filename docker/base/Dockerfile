FROM mattocci/radian-verse:4.5.1

ENV DEBIAN_FRONTEND=noninteractive

# Debug output to confirm TARGETPLATFORM (optional)
ARG TARGETPLATFORM
RUN echo "TARGETPLATFORM: ${TARGETPLATFORM}"

USER root

RUN apt-get update -q -y \
  && apt-get install --no-install-recommends --fix-missing -y \
    libmagick++-dev \
    cargo \
    rustc \
  && apt-get autoremove -y \
  && apt-get clean all

RUN mkdir -p  /opt/cmdstan

RUN R -q -e '\
  install.packages("cmdstanr", repos=c("https://mc-stan.org/r-packages/","https://cloud.r-project.org")); \
  cmdstanr::check_cmdstan_toolchain(); \
  cmdstanr::install_cmdstan(dir="/opt/cmdstan", quiet=TRUE, cores=2)'
