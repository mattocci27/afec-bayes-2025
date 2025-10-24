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
# Use a shared cache for faster builds; let renv use the default per-project library
ENV RENV_PATHS_CACHE=/opt/renv/cache \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE \
    RENV_INSTALL_PAK_ENABLED=TRUE \
    RENV_PROJECT=/home/rstudio/proj

# create writable cache + project dirs
RUN mkdir -p /opt/renv/cache \
 && chown -R rstudio:rstudio /opt/renv /home/rstudio

USER rstudio

# ---- Restore packages INTO the image (with BuildKit heredoc) ----
# Cache-mount ONLY the renv cache; the project library is written into the image layer.
RUN --mount=type=cache,target=/opt/renv/cache,uid=1000,gid=1000 <<'BASH'
set -euo pipefail

cat >/tmp/renv-restore.R <<'RSCRIPT'
options(repos = c(CRAN = "https://cloud.r-project.org"))
renv::consent(provided = TRUE)
renv::activate(project = Sys.getenv("RENV_PROJECT"))

cat("Project: ", renv::project(), "\n")
cat("Lib (before): ", renv::paths$library(), "\n")

dir.create(renv::paths$library(), recursive = TRUE, showWarnings = FALSE)
renv::restore(prompt = FALSE)

cat("Lib (after):  ", renv::paths$library(), "\n")

ip   <- rownames(installed.packages(lib.loc = renv::paths$library()))
need <- c("targets","ape")   # adjust this sentinel list as you like
miss <- setdiff(need, ip)
if (length(miss)) stop(paste("Missing after restore:", paste(miss, collapse=",")))

cat("OK: renv library populated at: ", renv::paths$library(), "\n", sep = "")
RSCRIPT

R --vanilla -q -f /tmp/renv-restore.R
BASH

USER root

# Keep ownership tidy (optional)
RUN chown -R rstudio:rstudio /home/rstudio

# Default workdir for sessions
WORKDIR /home/rstudio/proj
