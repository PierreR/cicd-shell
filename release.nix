# You can build this repository using Nix by running:
#
#     $ nix build -f release.nix project
#
# You can run the builded cicd command in a nix shell with:
#     $ nix run -r release.nix project
#
let
  pkgs = import ./share/pin.nix { };
  filter =  path: type:
    type != "symlink" && baseNameOf path != ".stack-work"
                      && baseNameOf path != "stack.yaml"
                      && baseNameOf path != ".git";


  cicd-shell = pkgs.haskell.lib.dontHaddock
    ( pkgs.haskellPackages.callCabal2nix
        "cicd-shell"
        (builtins.path { name = "cicd-shell"; inherit filter; path = ./.; } )
        { }
    );
  dockerTools = pkgs.dockerTools;
in
rec {
  docker = dockerTools.buildImage {
    name = "cicd-docker.repository.irisnet.be/cicd-shell";
    fromImage = dockerTools.buildImage {
      name = "bash";
      contents = pkgs.bashInteractive;
    };
    # config.Entrypoint = [ "${pkgs.bash}/bin/bash"];
    config.Entrypoint = [ "${project}/bin/cicd"];
    config.Env = [
      "PATH=/usr/bin:/usr/sbin:${pkgs.bash}/bin:${pkgs.jq}/bin:${pkgs.pepper}/bin:${pkgs.iputils}/bin:${pkgs.coreutils}/bin"
    ];
  };

  project = pkgs.buildEnv {
      name = "cicd-shell";

      paths = [
        cicd-shell
        pkgs.jq
        pkgs.pepper
      ];
  };

}
