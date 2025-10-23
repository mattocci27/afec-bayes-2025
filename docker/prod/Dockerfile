FROM mattocci/afec-bayes:base

RUN R -q -e 'install.packages(c("renv", "pak"), repos="https://cloud.r-project.org")'

# RUN R -q -e 'pak::pkg_install(c("clustermq"))'
# RUN R -q -e 'pak::pkg_install(c("brms", "targets", "bayesplot", "posterior", "tidybayes", "gifski", "animation"))'

RUN mkdir -p /home/rstudio/proj \
  && chown rstudio:rstudio /home/rstudio/proj

# 1) Put the project in place BEFORE restore
WORKDIR /home/rstudio/proj
COPY --chown=rstudio:rstudio . /home/rstudio/proj
RUN rm -f /home/rstudio/proj/.env

# 2) Prepare renv/pak and a CRAN mirror
ENV RENV_PATHS_CACHE=/opt/renv/cache \
    RENV_PATHS_LIBRARY=/home/rstudio/.cache/R/renv/library \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE
RUN mkdir -p /opt/renv/cache \
  && chown -R rstudio:rstudio /opt/renv \
  && mkdir -p /home/rstudio/.cache/R \
  && chown -R rstudio:rstudio /home/rstudio/.cache


# 3) Do a verbose restore (with pak backend) and show diagnostics on failure
ENV RENV_INSTALL_PAK_ENABLED=TRUE
USER rstudio
RUN --mount=type=cache,target=/home/rstudio/.cache/R,uid=1000,gid=1000 R --vanilla -e '\
  options(repos=c(CRAN="https://cloud.r-project.org")); \
  renv::consent(provided=TRUE); \
  renv::activate(); \
  # --- diagnostics --- \
  cat("RENV_INSTALL_PAK_ENABLED=", Sys.getenv("RENV_INSTALL_PAK_ENABLED"), "\n"); \
  tryCatch({ \
    # prefer calling renv:::renv_pak_enabled() if available, but fall back safely
    f <- get("renv_pak_enabled", envir = asNamespace("renv")); \
    cat("renv_pak_enabled()=", f(), "\n"); \
  }, error=function(e) cat("renv_pak_enabled(): <not available in this renv>\n")); \
  # --- restore --- \
  tryCatch(renv::restore(prompt=FALSE), error=function(e){ \
    message("---- renv diagnostics ----"); try(renv::diagnostics(), silent=TRUE); stop(e) \
  })'
USER root
