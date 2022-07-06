FROM rocker/shiny:4.0.5

MAINTAINER  Tanim tanim1109135@gmail.com

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev libssl-dev libssh2-1-dev libpq-dev \
    zlib1g-dev net-tools iputils-ping curl vim wget -y

RUN R -e 'install.packages(c("shiny", "shinydashboard", "RPostgreSQL", \
    "DBI", "plotly", "ggplot2", "DT", "lubridate", "dplyr", "stringr", \
    "tidyr", "shinyWidgets"), repos="http://cran.rstudio.com/")'




# docker build --tag=shiny-app:1.1 .
# docker login
# docker tag shiny-app:1.1 ktanim90/shiny-app:1.0
# docker push ktanim90/shiny-app:1.0
