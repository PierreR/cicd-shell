#!/usr/bin/env bash

if [ -z "$ZONE" ] ; then
    echo "ERROR: expecting a ZONE parameter";
    exit 1;
fi

SALT_USER=$(cat $HOME/.user_id)
if [ -z "$SALT_USER" ]; then
    read -p "Enter salt username : " SALT_USER
    SALT_USER=${SALT_USER}
fi

SALT_PASS=$(cat $HOME/.user_pwd)
if [ -z "$SALT_PASS" ]; then
    read -p "Enter salt password: " -s PWD
    SALT_PASS=$PWD
    echo ''
fi

STACK=$(cat $HOME/.user_stack)
if [ -z "$STACK" ]; then
    read -p "Enter your stack name: $STACK " STACK
    STACK=${STACK}
fi

case $ZONE in
    "testing")
        SALT_URL="https://saltmaster.sandbox.srv.cirb.lan:8000"
        ;;
    "staging")
        SALT_URL="https://salt.sta.srv.cirb.lan:8000"
        ;;
    "prod")
        SALT_URL="https://salt.prd.srv.cirb.lan:8000"
        ;;
    "dev")
        SALT_URL="https://salt.dev.srv.cirb.lan:8000"
        ;;
esac

case $ZONE in
    "testing")
        PGSERVER_URL="http://pgserver.sandbox.srv.cirb.lan/saltstack"
        ;;
    *)
        PGSERVER_URL="http://pgserver-cicd.prd.srv.cirb.lan/saltstack-${ZONE}"
        ;;
esac

cat <<EOF > ${ZONE}.nix
(import ./.) {
  zone         = "$ZONE";
  stack        = "$STACK";
  salt-user    = "$SALT_USER";
  salt-pass    = "$SALT_PASS";
  salt-url     = $SALT_URL;
  pgserver-url = $PGSERVER_URL;
}
EOF

echo "${PWD}/${ZONE}.nix created"
