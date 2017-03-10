{ salt-user, salt-pass, salt-url, zone }:
let
  stable = import <nixpkgs> { };
  src = stable.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (builtins.fromJSON (builtins.readFile ./.nixpkgs.json)) rev sha256;
  };

  pkgs = import src { };
  hghc = pkgs.haskellPackages;
  pepper = pkgs.pythonPackages.buildPythonApplication rec {
    name = "salt-pepper-${version}";
    version = "0.5.0";
    src = pkgs.fetchurl {
        url = "https://github.com/saltstack/pepper/releases/download/${version}/${name}.tar.gz";
        sha256 = "0gf4v5y1kp16i1na4c9qw7cgrpsh21p8ldv9r6b8gdwcxzadxbck";
    };
    doCheck = false;
  };
in
stable.stdenv.mkDerivation {
  name = "pepper-env";
  buildInputs = [ pepper stable.jq hghc.language-puppet ];
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
  alias pep="pepper"
  '';
}
