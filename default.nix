{ salt-user, salt-pass, salt-url, pgserver-url, puppetdb-url, zone, stack }:
with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "pepper-env";
  buildInputs = [ pepper jq haskellPackages.language-puppet];
  shellHook = ''
  export SALTAPI_USER="${salt-user}"
  export SALTAPI_PASS="${salt-pass}"
  export SALTAPI_URL="${salt-url}"
  export PGSERVER_URL="${pgserver-url}"
  export PUPPETDB_URL="${puppetdb-url}"
  export ZONE="${zone}"
  export STACK="${stack}"
  export SALTAPI_EAUTH=ldap
  source ./utils.sh
  export PS1="\n\[\033[1;32m\][salt ${zone}]$\[\033[0m\] "
  '';
}
