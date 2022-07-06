# bi



# install nginx


```

docker network create --driver=bridge --subnet=162.17.1.0/24 --gateway=162.17.1.1 ump_net

docker volume create --name bi_log_data --driver local --opt type=none --opt device=/data/bi/log --opt o=bind
docker volume create --name shiny_data --driver local --opt type=none --opt device=/data/bi/shiny --opt o=bind
docker volume create --name portainer_data --driver local --opt type=none --opt device=/data/portainer --opt o=bind

docker run --name=portainer -d -v /var/run/docker.sock:/var/run/docker.sock -v portainer_data:/data --net ump_net --ip 162.17.1.20 portainer/portainer-ce


```
