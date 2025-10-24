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
    RENV_INSTALL_PAK_ENABLED=TRUE \
    RENV_PROJECT=/home/rstudio/proj     # <— force project detection

# create writable dirs
RUN mkdir -p /opt/renv/cache /home/rstudio/renv-lib \
 && chown -R rstudio:rstudio /opt/renv /home/rstudio/renv-lib

USER rstudio

# Ensure library dir exists and is writable, cache-mount only the cache,
# then restore and assert key packages are present.
RUN --mount=type=cache,target=/opt/renv/cache,uid=1000,gid=1000 \
    R --vanilla -q -e '\
      options(repos = c(CRAN = "https://cloud.r-project.org")); \
      renv::consent(provided = TRUE); \
      renv::activate(project = Sys.getenv("RENV_PROJECT")); \
      cat("Project: ", renv::project(), "\n"); \
      cat("Lib (before): ", renv::paths$library(), "\n"); \
      dir.create(renv::paths$library(), recursive = TRUE, showWarnings = FALSE); \
      renv::restore(prompt = FALSE); \
      cat("Lib (after):  ", renv::paths$library(), "\n"); \
      ip <- rownames(installed.packages(lib.loc = renv::paths$library())); \
      need <- c("targets","ape"); \
      miss <- setdiff(need, ip); \
      if (length(miss)) { \
        cat("Installed (first 10): ", paste(head(ip, 10), collapse = ", "), "\n"); \
        stop("Missing after restore: ", paste(miss, collapse = ", ")); \
      } \
      cat("OK: renv library populated at: ", renv::paths$library(), "\n")'

USER root

# (optional) keep /home/rstudio as owner after restore
RUN chown -R rstudio:rstudio /home/rstudio

# Default workdir for the session
WORKDIR /home/rstudio/proj
