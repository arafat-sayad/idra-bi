FROM sayad1/shinyserver:1.0.0

MAINTAINER ________________

RUN addgroup --system app && adduser --system --ingroup app app
COPY ./src/*.R /srv/shiny-server/
COPY ./src/logo /srv/shiny-server/logo
COPY ./src/www /srv/shiny-server/www
COPY ./shiny-server.conf /etc/shiny-server/
COPY ./src/ump_bi_credential.sqlite /srv/shiny-server/

RUN  chmod -R o+rwx /srv/shiny-server/

#COPY ./data /srv/shiny-server/data


RUN R --version
RUN Rscript /srv/shiny-server/install_packages.R

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]


# docker stop bi_web && docker rm bi_web
# docker rmi bi:1.0
# docker build --tag=bi:1.0 .
# docker run --name bi_web -it -v /home/tanim/Documents/SECL/idra-doer/bi/01-web/data:/srv/shiny-server/data --publish 3838:3838 bi:1.0
