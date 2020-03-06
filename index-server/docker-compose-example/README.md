# Ergvein index server docker-compose example

Use official docker image [ergvein/ergvein-index-server](https://hub.docker.com/r/ergvein/ergvein-index-server) 

Adjust connection settings for bitcoin and ergo nodes in config.yml and run:

```
docker-compose up
```

Ergvein index server will listen on all network interfaces tcp 8080.

Note that you need to have running bitcoin/ergo nodes For bitcoin node, btc ip from config.yml should be allowed in bitcoin.conf by adding a line "rpcbind=[ip]"
