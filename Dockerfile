# docker pull rocker/tidyverse:4.3
FROM rocker/tidyverse:4.3

# install dependencies
RUN apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev

# install R packages
RUN R -e "install.packages(c('dplyr', 'dlnm', 'mvmeta', 'splines', 'tsModel', 'config', 'zeallot', 'lubridate', 'FluMoDL'))"
RUN R -e "install.packages('yaml')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('plumber')"

# copy everything from the current directory into the container
COPY . /app

# set the working directory
WORKDIR /app

# install the climatehealth package
RUN R -e "devtools::install()"

# make the API run script executable
RUN chmod +x /app/api/run_api.R

# expose the port
EXPOSE 8000

# run the API
CMD ["Rscript", "/app/api/run_api.R"]
