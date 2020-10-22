{ zone }:
let
  pkgs = import ./pin.nix {};
  completion_file = ./completion.sh;
in
pkgs.stdenv.mkDerivation {
  name = "pepper-env";
  shellHook = ''
    source ${completion_file} ${zone}
    export PS1="\n\[\033[1;32m\][cicd ${zone}]$\[\033[0m\] "
    unalias -a
    alias facts="cicd ${zone} facts"
    alias du="cicd ${zone} du"
    alias data="cicd ${zone} data"
    alias stats="cicd ${zone} stats"
    alias ping="cicd ${zone} ping"
    alias runpuppet="cicd ${zone} runpuppet"
    alias sync="cicd ${zone} sync"
    alias gentags="cicd ${zone} gentags"
    alias service="cicd ${zone} service"
    alias state="cicd ${zone} state"
    alias setfacts="cicd ${zone} setfacts"
    alias doc="cicd doc"
    alias foreman="cicd ${zone} foreman"
    alias pep="pepper"
  '';
}
