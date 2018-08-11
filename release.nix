# You can build this repository using Nix by running:
#
#     $ nix build -f release.nix project
#
# You can run the builded cicd command in a nix shell with:
#     $ nix run -r release.nix project
#
let
  hoverlays = self: super:
      let
        hlib = super.haskell.lib;
        lib = super.lib;
        filter =  path: type: type != "link" && baseNameOf path != ".stack-work";
      in
      {
        haskellPackages = super.haskellPackages.override {
          overrides = hself: hsuper: rec {
            project = hlib.overrideCabal
              ( hsuper.callPackage ./cicd-shell.nix { })
              ( csuper: { executableSystemDepends = [ self.jq self.pepper ];
                          src =  lib.cleanSourceWith { inherit filter ; src = lib.cleanSource csuper.src;};
                        }
              );
        };
      };
  };
  pkgs = import ./share/pin.nix { overlays = [ hoverlays];};
  dockerTools = pkgs.dockerTools;
in
rec {
  project =  pkgs.haskell.lib.justStaticExecutables ( pkgs.haskellPackages.project );

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
}
