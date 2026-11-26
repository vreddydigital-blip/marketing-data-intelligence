# Production container image for Marketing Data Intelligence.
# Base image includes Shiny, tidyverse, and pandoc (needed for HTML reports).
FROM rocker/shiny-verse:4.4.1

# System packages for R packages that compile from source. TeX is intentionally
# excluded: PDF export needs it only when explicitly installed (see
# docs/DEPLOYMENT.md); HTML/Excel/CSV reports work without it.
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libicu-dev \
    make \
    pandoc \
  && rm -rf /var/lib/apt/lists/*

# R packages not shipped by the base image. Versions resolve to current CRAN;
# the repository's renv.lock documents the tested reference stack.
RUN Rscript -e 'options(warn = 1); install.packages(c(
      "bslib", "shinyWidgets", "shinyjs",
      "readxl", "janitor", "data.table",
      "plotly", "DT",
      "broom", "rstatix", "car",
      "tidymodels", "ranger", "glmnet",
      "forecast", "tsibble", "fable",
      "DBI", "RSQLite",
      "openxlsx", "writexl",
      "rmarkdown"
    ), Ncpus = 4)'

# Application code
RUN mkdir -p /srv/marketing-data-intelligence
COPY . /srv/marketing-data-intelligence

# Runtime configuration: persistent storage lives at /data (mount a volume).
ENV MDI_DATA_DIR=/data \
    MDI_HOST=0.0.0.0 \
    MDI_PORT=3838
VOLUME ["/data"]
EXPOSE 3838

WORKDIR /srv/marketing-data-intelligence
CMD ["Rscript", "run_app.R"]
