FROM mattocci/afec-bayes:base

RUN R -q -e 'install.packages(c("renv","pak"), repos="https://cloud.r-project.org")'

# Make the project dir with correct owner so WORKDIR won’t create it as root
RUN mkdir -p /home/rstudio/proj && chown rstudio:rstudio /home/rstudio/proj

USER rstudio
WORKDIR /home/rstudio/proj
COPY . .

ENV RENV_INSTALL_PAK_ENABLED=TRUE

# Restore packages INTO the image (no cache mount; simplest)
RUN R --vanilla -q -e '\
  options(repos = c(CRAN = "https://cloud.r-project.org")); \
  renv::consent(provided = TRUE); \
  renv::restore(prompt = FALSE)'
USER root
