{ salt-user, salt-pass, salt-url, zone  }:
with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "pepper-env";
  buildInputs = [ pepper jq haskellPackages.language-puppet_1_3_4];
  shellHook = ''
  export SALTAPI_USER="${salt-user}"
  export SALTAPI_PASS="${salt-pass}"
  export SALTAPI_URL="${salt-url}"
  export SALTAPI_EAUTH=ldap
  export ZONE="${zone}"
  export PS1="\n\[\033[1;32m\][salt ${zone}]$\[\033[0m\] "
  alias cicd="cicd $ZONE"
  '';
}
