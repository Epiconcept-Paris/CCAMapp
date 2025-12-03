FROM rocker/verse:4.4.2
RUN apt-get update && apt-get install -y  cmake gdal-bin libabsl-dev libgdal-dev libgeos-dev libicu-dev libpng-dev libproj-dev libsqlite3-dev libssl-dev libudunits2-dev make pandoc xz-utils zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.2.3")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.11.1")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "1.0-21")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("duckdb",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.2.3")'
RUN Rscript -e 'remotes::install_version("readxl",upgrade="never", version = "1.4.5")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_version("duckplyr",upgrade="never", version = "1.1.3")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.34.0")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.17.8")'
RUN Rscript -e 'remotes::install_github("walkerke/mapgl@a058a4ce95e9344dcc8ab057a0526491e3e34a06")'
RUN Rscript -e 'remotes::install_github("sfirke/janitor@81702b6ed2b97a143319700a8edf48e8e4cce9cd")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(CCAMapp);CCAMapp::run_app()"
