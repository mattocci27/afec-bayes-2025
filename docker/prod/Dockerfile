FROM mattocci/afec-bayes:base

RUN R -q -e 'install.packages(c("renv","pak"), repos="https://cloud.r-project.org")'

# Make the project dir with correct owner so WORKDIR won’t create it as root
RUN mkdir -p /home/rstudio/proj && chown rstudio:rstudio /home/rstudio/proj

USER rstudio
WORKDIR /home/rstudio/proj
COPY --chown=rstudio:rstudio . /home/rstudio/proj

# BEFORE running renv::restore()
ENV RENV_BOOTSTRAP_FROM=system \
    RENV_INSTALL_PAK_ENABLED=TRUE \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE \
    RENV_CONFIG_HARD_LINKS_ENABLED=FALSE

RUN mkdir -p /home/rstudio/proj/renv/library

# Restore packages INTO the image (no cache mount; simplest)
RUN R --vanilla -q -e '\
  options(repos = c(CRAN = "https://cloud.r-project.org")); \
  renv::consent(provided = TRUE); \
  renv::restore(prompt = FALSE)'
USER root
