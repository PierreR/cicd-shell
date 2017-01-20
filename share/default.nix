{ salt-user, salt-pass, salt-url, zone  }:
let
  bootstrap = import <nixpkgs> { };
in
with import (bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (builtins.fromJSON (builtins.readFile ./.nixpkgs.json)) rev sha256;
  }) { };
stdenv.mkDerivation {
  name = "pepper-env";
  buildInputs = [ pepper jq haskellPackages.language-puppet_1_3_4 ];
  shellHook = ''
  export SALTAPI_USER="${salt-user}"
  export SALTAPI_PASS="${salt-pass}"
  export SALTAPI_URL="${salt-url}"
  export SALTAPI_EAUTH=ldap
  export ZONE="${zone}"
  export PS1="\n\[\033[1;32m\][cicd ${zone}]$\[\033[0m\] "
  alias cicd="cicd $ZONE"
  alias pep="pepper"
  '';
}
