#!/bin/bash 
docker build -t eugraceful/grace .
docker tag eugraceful/grace eugraceful/grace:$(git rev-parse --short HEAD)
docker login -u "eugraceful"
docker push eugraceful/grace:latest
docker push eugraceful/grace:$(git rev-parse --short HEAD)
