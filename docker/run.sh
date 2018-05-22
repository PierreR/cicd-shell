#! /usr/bin/env bash

set -e
if [ -z $1 ]; then echo "Please specify a command"; exit 1; fi

docker run --network host  -v /vagrant:/vagrant -v $HOME/.local/share/cicd:/.local/share/cicd --rm -it pi3r/cicd-shell ${@:1}
