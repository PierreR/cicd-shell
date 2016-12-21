#!/usr/bin/env bash

zone=${1:-ZONE}
if [ -z "$zone" ] ; then
    echo "ERROR: expecting a ZONE parameter";
    exit 1;
fi

SALT_USER=$(cat $HOME/.user_id)
if [ -z "$SALT_USER" ]; then
    read -p "Enter salt username : " SALT_USER
    SALT_USER=${SALT_USER}
fi

case $zone in
    "testing")
        SALT_URL="https://saltmaster.sandbox.srv.cirb.lan:8000"
        PGSERVER_URL="http://pgserver.sandbox.srv.cirb.lan/saltstack"
        PUPPETDB_URL="http://puppetdb.sandbox.srv.cirb.lan:8080"
        ;;
    "dev")
        SALT_URL="https://salt.dev.srv.cirb.lan:8000"
        PGSERVER_URL="http://pgserver-cicd.prd.srv.cirb.lan/saltstack-${zone}"
        PUPPETDB_URL="http://puppetdb.prd.srv.cirb.lan:8080"
        ;;
    "staging")
        SALT_URL="https://salt.sta.srv.cirb.lan:8000"
        PGSERVER_URL="http://pgserver-cicd.prd.srv.cirb.lan/saltstack-${zone}"
        PUPPETDB_URL="http://puppetdb.prd.srv.cirb.lan:8080"
        ;;
    "prod")
        SALT_URL="https://salt.prd.srv.cirb.lan:8000"
        PGSERVER_URL="http://pgserver-cicd.prd.srv.cirb.lan/saltstack"
        PUPPETDB_URL="http://puppetdb.prd.srv.cirb.lan:8080"
        ;;
esac

cat <<EOF > ${zone}.nix
{user_pwd}:
(import ./.) {
  zone         = "$zone";
  salt-user    = "$SALT_USER";
  salt-pass    = "${user_pwd}";
  salt-url     = $SALT_URL;
  pgserver-url = $PGSERVER_URL;
  puppetdb-url = $PUPPETDB_URL;
}
EOF

echo "${PWD}/${zone}.nix created"
