#!/usr/bin/env bash

NETWORK_ID=$(echo `docker network ls | grep ump_net | awk '{print $1}'`)

if [ ! -z "${NETWORK_ID}" ]
then
    echo `docker network ls | grep ump_net`
else
    docker network create --driver=bridge --subnet=162.17.1.0/24 --gateway=162.17.1.1 ump_net
    echo "ump_net network created"
    echo `docker network ls | grep ump_net`
fi

docker stop $(docker ps -a | grep con_ump_bi | awk '{print $1}')
docker rm $(docker ps -a | grep con_ump_bi | awk '{print $1}')
docker rmi $(docker images | grep img_ump_bi | tr -s ' ' | cut -d ' ' -f 3) -f
docker rmi $(docker images | grep none | tr -s ' ' | cut -d ' ' -f 3) -f
docker build --tag=img_ump_bi .
docker run --name=con_ump_bi -it -d -v /opt/bi/data:/srv/shiny-server/data --net ump_net --ip 162.17.1.2 -p 3838:3838 img_ump_bi

# docker exec -it con_ump_bi sh
