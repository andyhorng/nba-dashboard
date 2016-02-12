#!/usr/bin/env bash

set -xe

LOGGER=$(kubectl get -o json pods -l app=nsq-logger | jq -r .items[0].metadata.name)
kubectl exec -i $LOGGER -c python -- rm dump.tar.gz || true
kubectl exec -i $LOGGER -c python -- pip install rethinkdb==1.16.0-3
kubectl exec -i $LOGGER -c python -- rethinkdb-dump -c rethinkdb-driver -f dump.tar.gz
kubectl exec -i $LOGGER -c python -- cat dump.tar.gz | cat > dump-data.tar.gz

docker exec -i nbadashboard_python_1 pip install rethinkdb==1.16.0-3
docker cp ./dump-data.tar.gz nbadashboard_python_1:/
docker exec -i nbadashboard_python_1 rethinkdb-restore /dump-data.tar.gz -c rethinkdb-driver --force
