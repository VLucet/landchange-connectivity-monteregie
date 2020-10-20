# ------------------------------------------------------------------------------
## To build:
# docker build Dockerfiles -t vlucet/land_con_monteregie:3.6.2-1
## To run:
## Within Rstudio:
# docker run -d -p 8787:8787 -e DISABLE_AUTH=true vlucet/land_con_monteregie:3.6.2-1
## Interactive:
# docker run -it -e DISABLE_AUTH=true vlucet/land_con_monteregie:3.6.2-1 /bin/bash && cd ~/land_con_monteregie
# For CC1:
# docker run -it -e DISABLE_AUTH=true -v "$(pwd):/home/rstudio/land_con_monteregie/" -v "/home/ubuntu/data/val/current_density_dump:/home/rstudio/current_density_2" --shm-size 50G vlucet/land_con_monteregie:3.6.2-1 /bin/bash && cd ~/land_con_monteregie
# ------------------------------------------------------------------------------

# Building on top of: rocker/geospatial:3.6.2
FROM rocker/geospatial:3.6.2

# Maintainer info
MAINTAINER Valentin Lucet <valentin.lucet@gmail.com>

# ENV vars
ENV PATH="$PATH:/home/rstudio/julia-1.4.2/bin"
ENV RENV_VERSION 0.10.0

# Install System dependencies
RUN apt-get -y update -qq \
  && apt-get install -y --no-install-recommends \
    libgsl-dev=2.5+dfsg-6 \
    mono-devel=5.18.0.240+dfsg-3 \
    grass-dev=7.6.0-1 \
    curl=7.64.0-4+deb10u1 \ 
    cargo \
    gdal-bin=2.4.0+dfsg-1+b1 \
    python-gdal=2.4.0+dfsg-1+b1 \
    bc=1.07.1-2+b1 \
    nano=3.2-3 \
    htop=2.2.0-1+b1 \
    # OpenCV req
    cmake \
    unzip \
    pkg-config \
    libjpeg-dev \
    libpng-dev \
    libtiff-dev \
    libavcodec-dev \
    libavformat-dev \
    libswscale-dev \
    libv4l-dev \
    libxvidcore-dev \
    libx264-dev \
    libatlas-base-dev \
    gfortran \
    python3-dev \
    python3-pip
    # Latex req
    #pandoc \
    #texlive \
    #texlive-latex-extra \
    #texlive-extra-utils \
    #texlive-fonts-extra \
    #texlive-bibtex-extra \
    #biber \ 
    #latexmk \
    #texlive-science
    
# Julia install
ENV JULIA_DEPOT_PATH=/opt/julia
ENV JULIA_PKGDIR=/opt/julia

# Install julia and custom version of circuitscape
RUN curl -L https://julialang-s3.julialang.org/bin/linux/x64/1.4/julia-1.4.2-linux-x86_64.tar.gz --output /home/rstudio/julia-1.4.2-linux-x86_64.tar.gz \
  && tar -xzf /home/rstudio/julia-1.4.2-linux-x86_64.tar.gz -C /home/rstudio/ \
  && julia -e 'import Pkg; Pkg.activate("."); Pkg.instantiate(); Pkg.precompile()' \
  && chown -R rstudio /home/rstudio/julia-1.4.2 \ 
  && chown -R rstudio /opt/julia

# Install syncrosim and download package file
RUN mkdir /home/rstudio/syncrosim_2_10 \
  && chown -R rstudio /home/rstudio/syncrosim_2_10 \
  && curl -L https://syncrosim.com/download-id/19813/ --output /home/rstudio/syncrosim_2_10.zip \
  && unzip /home/rstudio/syncrosim_2_10.zip -d /home/rstudio/syncrosim_2_10 \
  && mkdir /home/rstudio/syncrosim_packages \
  && chown -R rstudio /home/rstudio/syncrosim_packages \
  && curl -L https://github.com/ApexRMS/stsim/releases/download/3.2.17/stsim-3-2-17.ssimpkg --output /home/rstudio/syncrosim_packages/stsim-3-2-17.ssimpkg

# OpenCV / python
RUN cd /home/rstudio \
  && git clone https://github.com/opencv/opencv.git \
  && git clone https://github.com/opencv/opencv_contrib.git \
  && cd /home/rstudio/opencv \
  && mkdir build \
  && cd build \
  && sudo cmake -D CMAKE_BUILD_TYPE=RELEASE \
  -D CMAKE_INSTALL_PREFIX=/usr/local \
  -D OPENCV_ENABLE_NONFREE=ON \
  -D OPENCV_EXTRA_MODULES_PATH=/home/rstudio/opencv_contrib/modules \
  -D PYTHON_INCLUDE_DIR=/usr/include/python3.7 \
  -D PYTHON3_EXECUTABLE=/usr/bin/python3 .. \
	&& make -j4 \
	&& sudo make install \
	&& pip3 install numpy matplotlib pandas scikit-image scipy

# Download repo
RUN mkdir /home/rstudio/land_con_monteregie \
  && chown -R rstudio /home/rstudio/land_con_monteregie \
  && git clone https://github.com/VLucet/landchange-connectivity-monteregie.git /home/rstudio/land_con_monteregie

# Move to the working directory
WORKDIR /home/rstudio/land_con_monteregie

# Set up renv and Restore renv library
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
RUN R -e "renv::restore()"

# Set up links
RUN chmod +x landcon.sh \
  && ln -s /home/rstudio/land_con_monteregie/landcon.sh /usr/bin/
  
# ------------------------------------------------------------------------------
# Set up git lfs 
#RUN curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | bash \
#  && apt-get install git-lfs \
#  && git lfs install \
#  && git lfs fetch \
#  && git lfs checkout
# ------------------------------------------------------------------------------
## With renv cache, has not worked so far: 
# RENV_PATHS_CACHE_HOST=/opt/local/renv/cache
# RENV_PATHS_CACHE_CONTAINER=/renv/cache
#  docker run -it -e DISABLE_AUTH=true -e "RENV_PATHS_CACHE=${RENV_PATHS_CACHE_CONTAINER}" -v "$(pwd)/config/:/home/rstudio/land_con_monteregie/config/" -v "$(pwd)/outputs:/home/rstudio/land_con_monteregie/outputs/" -v "$(pwd)/libraries:/home/rstudio/land_con_monteregie/libraries" -v "$(pwd)/data:/home/rstudio/land_con_monteregie/data" -v "${RENV_PATHS_CACHE_HOST}:${RENV_PATHS_CACHE_CONTAINER}" --shm-size 50G vlucet/land_con_monteregie:3.6.2-1 /bin/bash && cd ~/land_con_monteregie
## For compute cloud 1:
# docker run -it -e DISABLE_AUTH=true -v "$(pwd)/config/:/home/rstudio/land_con_monteregie/config/" -v "$(pwd)/outputs:/home/rstudio/land_con_monteregie/outputs/" -v "$(pwd)/libraries:/home/rstudio/land_con_monteregie/libraries" -v "$(pwd)/data:/home/rstudio/land_con_monteregie/data" -v "/home/ubuntu/data/val/current_density_dump:/home/rstudio/current_density_2" --shm-size 50G vlucet/land_con_monteregie:3.6.2-1 /bin/bash && cd ~/land_con_monteregie
# ------------------------------------------------------------------------------
