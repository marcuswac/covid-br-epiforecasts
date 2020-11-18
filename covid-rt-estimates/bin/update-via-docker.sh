#!/bin/bash


## Remove old containers
sudo docker stop covidrtestimates
sudo docker rm covidrtestimates

## Update estimates in newly built docker container
## This will use all cores available to docker by default
sudo docker run -d --user rstudio --mount type=bind,source=$(pwd),target=/home/rstudio/covid-rt-estimates --name covidrtestimates covidrtestimates /bin/bash bin/update-estimates.sh

