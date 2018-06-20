#! /usr/bin/env bash

set -e
if [ -z $1 ]; then echo "Please specify a command"; exit 1; fi

docker run --network host  -v /vagrant:/vagrant -v $HOME/.local/share/cicd:/.local/share/cicd --rm -it cicd-docker.repository.irisnet.be/cicd-shell ${@:1}
