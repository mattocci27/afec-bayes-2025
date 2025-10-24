FROM mattocci/afec-bayes:base

# Use bash (optional; safe either way)
SHELL ["/bin/bash", "-lc"]

# 1) Install renv + pak (pak used by renv during restore)
RUN R -q -e 'install.packages(c("renv","pak"), repos="https://cloud.r-project.org")'

# 2) Put your project (with renv.lock + renv/activate.R) in place
WORKDIR /home/rstudio/proj
COPY --chown=rstudio:rstudio . /home/rstudio/proj
RUN rm -f /home/rstudio/proj/.env

# 3) Configure renv to use pak and a shared cache (optional cache path)
ENV RENV_INSTALL_PAK_ENABLED=TRUE \
    RENV_PATHS_CACHE=/opt/renv/cache \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE

# 4) Create cache dir with proper perms
RUN mkdir -p /opt/renv/cache && chown -R rstudio:rstudio /opt/renv /home/rstudio

USER rstudio

# 5) Restore packages INTO the image
#    (pak will be used automatically because RENV_INSTALL_PAK_ENABLED=TRUE)
#    Cache-mount speeds up builds, but is optional.
RUN --mount=type=cache,target=/opt/renv/cache,uid=1000,gid=1000 \
    R --vanilla -q -e '\
      options(repos=c(CRAN="https://cloud.r-project.org")); \
      renv::consent(provided=TRUE); \
      renv::restore(prompt=FALSE)'

USER root

# Keep ownership tidy (optional)
RUN chown -R rstudio:rstudio /home/rstudio

# Default working directory
WORKDIR /home/rstudio/proj
