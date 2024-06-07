# build the Docker image from the base image 'rocker/r-ver'
# this installs Ubuntu 20.04 OS and a recent version of R
FROM rocker/r-ver:4

# add the maintainer of this Docker image (this should be you in this case)
LABEL maintainer "Adrian Hordyk <adrian@bluematterscience.com>"

# system libraries of general use
RUN apt-get update && apt-get install -y \
sudo \
build-essential \
git \
libicu-dev \
pandoc \
pandoc-citeproc \
zlib1g-dev

# install basic shiny functionality to R, including remotes to install from GitHub
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'remotes'), repos='https://cloud.r-project.org/')"


RUN R -e "remotes::install_github('blue-matter/MSEtool', dependencies=TRUE)"
RUN R -e "remotes::install_github('blue-matter/openMSE', dependencies=TRUE)"

# install R package from shiny_live branch
RUN R -e "options(timeout=9999999); remotes::install_github(c('ICCAT/nswo-mse'), 'shiny_live', dependencies=TRUE)"

# instruct Docker to expose port 3838 to the outside world
# (otherwise it will not be possible to connect to the Shiny application)
EXPOSE 3838

# launch the Shiny app when the container is started
CMD ["R", "-e", "SWOMSE::Shiny(host='0.0.0.0', port=3838)"]
