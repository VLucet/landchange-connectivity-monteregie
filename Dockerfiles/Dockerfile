# -----
## To build:
# docker build Dockerfiles -t vlucet/land_con_monteregie:3.6.2-1
## To run:
## Within Rstudio:
# docker run -d -p 8787:8787 -e DISABLE_AUTH=true -e PATH="$PATH:/home/rstudio/julia-1.3.1/bin" vlucet/land_con_monteregie:3.6.2-1
## Interactive:
# docker run -it -u rstudio -e DISABLE_AUTH=true vlucet/land_con_monteregie:3.6.2-1 /bin/bash && cd ~/land_con_monteregie
## With data volume: 
#  docker run -it -e DISABLE_AUTH=true -v "$(pwd)/config/:/home/rstudio/land_con_monteregie/config/" -v "$(pwd)/outputs:/home/rstudio/land_con_monteregie/outputs/" -v "$(pwd)/libraries:/home/rstudio/land_con_monteregie/libraries" -v "$(pwd)/data:/home/rstudio/land_con_monteregie/data" --shm-size 10G vlucet/land_con_monteregie:3.6.2-1 /bin/bash && cd ~/land_con_monteregie
# -----

# Building on top of: rocker/geospatial:3.6.2
FROM rocker/geospatial:3.6.2

# Maintainer info
MAINTAINER Valentin Lucet <valentin.lucet@gmail.com>

# ENV vars
ENV PATH="$PATH:/home/rstudio/julia-1.3.1/bin"
ENV RENV_VERSION 0.10.0

# Install System dependencies
RUN apt-get -y update -qq \
  && apt-get install -y --no-install-recommends \
    libgsl-dev=2.5+dfsg-6 \
    mono-devel=5.18.0.240+dfsg-3 \
    grass-dev=7.6.0-1 \
    curl=7.64.0-4+deb10u1 \ 
    cargo=0.35.0-2 \
    gdal-bin=2.4.0+dfsg-1+b1 \
    python-gdal=2.4.0+dfsg-1+b1 \
    bc=1.07.1-2+b1 \
    nano=3.2-3 \
    htop=2.2.0-1+b1
    
# Julia install
ENV JULIA_DEPOT_PATH=/opt/julia
ENV JULIA_PKGDIR=/opt/julia

# Install julia and custom version of circuitscape
RUN curl -L https://julialang-s3.julialang.org/bin/linux/x64/1.3/julia-1.3.1-linux-x86_64.tar.gz --output /home/rstudio/julia-1.3.1-linux-x86_64.tar.gz \
  && tar -xzf /home/rstudio/julia-1.3.1-linux-x86_64.tar.gz -C /home/rstudio/ \
  && julia -e 'import Pkg; Pkg.activate("."); Pkg.instantiate(); Pkg.precompile()' \
  && chown -R rstudio /home/rstudio/julia-1.3.1 \ 
  && chown -R rstudio /opt/julia

# Install syncrosim and download package file
RUN mkdir /home/rstudio/syncrosim_2_10 \
  && chown -R rstudio /home/rstudio/syncrosim_2_10 \
  && curl -L https://syncrosim.com/download-id/19813/ --output /home/rstudio/syncrosim_2_10.zip \
  && unzip /home/rstudio/syncrosim_2_10.zip -d /home/rstudio/syncrosim_2_10 \
  && mkdir /home/rstudio/syncrosim_packages \
  && chown -R rstudio /home/rstudio/syncrosim_packages \
  && curl -L https://github.com/ApexRMS/stsim/releases/download/3.2.17/stsim-3-2-17.ssimpkg --output /home/rstudio/syncrosim_packages/stsim-3-2-17.ssimpkg

# Download repo
RUN mkdir /home/rstudio/land_con_monteregie \
  && chown -R rstudio /home/rstudio/land_con_monteregie \
  && git clone https://github.com/VLucet/landchange-connectivity-monteregie.git /home/rstudio/land_con_monteregie

# Move to the working directory
WORKDIR /home/rstudio/land_con_monteregie

# Set up renv and Restore renv library
  RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
  RUN R -e 'renv::restore()'

# -----
# RENV_PATHS_CACHE_HOST=/opt/local/renv/cache
# RENV_PATHS_CACHE_CONTAINER=/renv/cache
# docker run --rm \
#     -e "RENV_PATHS_CACHE=${RENV_PATHS_CACHE_CONTAINER}" \
#     -v "${RENV_PATHS_CACHE_HOST}:${RENV_PATHS_CACHE_CONTAINER}" \
#     -p 14618:14618 \
#     R --slave -e 'renv::restore()'
# -----