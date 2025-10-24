# Base image with system deps you already maintain
FROM mattocci/afec-bayes:base

# ---- R tooling ----
RUN R -q -e 'install.packages(c("renv", "pak"), repos="https://cloud.r-project.org")'

# ---- Project layout ----
WORKDIR /home/rstudio/proj
COPY --chown=rstudio:rstudio . /home/rstudio/proj
# avoid leaking local env config into the image
RUN rm -f /home/rstudio/proj/.env

# ---- renv paths & behavior ----
ENV RENV_PATHS_CACHE=/opt/renv/cache \
    RENV_PATHS_LIBRARY=/home/rstudio/renv-lib \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE \
    RENV_INSTALL_PAK_ENABLED=TRUE

# create writable dirs
RUN mkdir -p /opt/renv/cache /home/rstudio/renv-lib \
 && chown -R rstudio:rstudio /opt/renv /home/rstudio/renv-lib

USER rstudio

# ---- Restore packages INTO the image ----
# Cache-mount only the renv CACHE (safe to skip committing),
# NOT the library, so the library is baked into the layer.
RUN --mount=type=cache,target=/opt/renv/cache,uid=1000,gid=1000 \
    R --vanilla -e '\
      options(repos = c(CRAN = "https://cloud.r-project.org")); \
      renv::consent(provided = TRUE); \
      renv::activate(); \
      tryCatch({ \
        message("pak enabled: ", tryCatch(get("renv_pak_enabled", asNamespace("renv"))(), error=function(e) NA)); \
        renv::restore(prompt = FALSE) \
      }, error = function(e) { \
        message("---- renv diagnostics ----"); try(renv::diagnostics(), silent = TRUE); stop(e) \
      })'

USER root

# (optional) keep /home/rstudio as owner after restore
RUN chown -R rstudio:rstudio /home/rstudio

# Default workdir for the session
WORKDIR /home/rstudio/proj
