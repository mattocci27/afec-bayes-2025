FROM mattocci/afec-bayes:base

RUN R -q -e 'install.packages(c("renv","pak"), repos="https://cloud.r-project.org")'

WORKDIR /home/rstudio/proj
COPY --chown=rstudio:rstudio . /home/rstudio/proj

USER rstudio
# ENV RENV_PATHS_CACHE=/opt/renv/cache \
ENV RENV_INSTALL_PAK_ENABLED=TRUE

# RUN mkdir -p /opt/renv/cache && chown -R rstudio:rstudio /opt/renv

# RUN R -e "renv::restore()"

# Restore packages INTO the image (no cache mount; simplest)
RUN R --vanilla -q -e '\
  options(repos = c(CRAN = "https://cloud.r-project.org")); \
  renv::consent(provided = TRUE); \
  renv::restore(prompt = FALSE)'

# # Use BuildKit cache mount for speed; restore into image
# RUN --mount=type=cache,target=/opt/renv/cache,uid=1000,gid=1000 \
#     R -q -e 'options(repos=c(CRAN="https://cloud.r-project.org")); renv::consent(provided=TRUE); renv::restore()'
# WORKDIR /home/rstudio/proj

USER root
