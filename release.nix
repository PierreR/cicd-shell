# You can build this repository using Nix by running:
#
#     $ nix build -f release.nix project
#
# You can run the builded cicd command in a nix shell with:
#     $ nix run -r release.nix project
#
let
  overlay = self: super:
    let
      hlib = super.haskell.lib;
      lib = super.lib;
      filter =  path: type:
                  type != "symlink" && baseNameOf path != ".stack-work"
                                    && baseNameOf path != "stack.yaml"
                                    && baseNameOf path != ".git";
    in
    {
      haskellPackages = super.haskellPackages.override {
        overrides = hself: hsuper: rec {
          cicd-shell = hlib.overrideCabal
            ( hsuper.callPackage ./cicd-shell.nix { })
            ( csuper: { executableSystemDepends = [ self.jq self.pepper ];
                        src = builtins.path { name = "cicd-shell"; inherit filter; path = csuper.src;};
                      }
            );
        };
      };
      cicd-shell = hlib.justStaticExecutables self.haskellPackages.cicd-shell;
  };
  pkgs = import ./share/pin.nix { config = {}; overlays = [ overlay ];};
  dockerTools = pkgs.dockerTools;
in
rec {
  project = pkgs.buildEnv {
      name = "cicd-shell";

      paths = [
        pkgs.cicd-shell
        pkgs.jq
        pkgs.pepper
      ];
  };

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
