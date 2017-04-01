{ salt-user, salt-pass, salt-url, zone }:
let
  _pkgs = import <nixpkgs> {};
  pkgs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/08c87eed00c7478a03d965570feb50c8f317ce5f.tar.gz) {}; # at 2017-03-29
  hghc = pkgs.haskellPackages;
in
pkgs.stdenv.mkDerivation {
  name = "pepper-env";
  buildInputs = [ pkgs.pepper _pkgs.jq hghc.language-puppet ];
  shellHook = ''
  export SALTAPI_USER="${salt-user}"
  export SALTAPI_PASS="${salt-pass}"
  export SALTAPI_URL="${salt-url}"
  export SALTAPI_EAUTH=ldap
  export ZONE="${zone}"
  export PS1="\n\[\033[1;32m\][cicd ${zone}]$\[\033[0m\] "
  unalias -a
  alias facts="cicd ${zone} facts"
  alias du="cicd ${zone} du"
  alias data="cicd ${zone} data"
  alias stats="cicd ${zone} stats"
  alias ping="cicd ${zone} ping"
  alias runpuppet="cicd ${zone} runpuppet"
  alias sync="cicd ${zone} sync"
  alias result="cicd ${zone} result"
  alias gentags="cicd ${zone} gentags"
  alias service="cicd ${zone} service"
  alias pep="pepper"
  '';
}
