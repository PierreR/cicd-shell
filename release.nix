# You can build this repository using Nix by running:
#
#     $ nix build -f release.nix project
#
# You can run the builded cicd command in a nix shell with:
#     $ nix run -r release.nix project
#
{
  pkgs ? import ./share/pin.nix { }
}:
let
  filter =  path: type:
    type != "symlink" && baseNameOf path != ".stack-work"
                      && baseNameOf path != "stack.yaml"
                      && baseNameOf path != "stack.yaml.lock"
                      && baseNameOf path != "dist-newstyle"
                      && baseNameOf path != "cabal.project.local"
                      && baseNameOf path != ".envrc"
                      && baseNameOf path != "salt.nix"
                      && baseNameOf path != ".git";


  # dhall = pkgs.haskell.lib.dontCheck (pkgs.haskellPackages.dhall_1_19_1.override {
  #           megaparsec = pkgs.haskellPackages.megaparsec_7_0_4;
  #           repline = pkgs.haskellPackages.repline_0_2_0_0;
  #         });
  # neat-interpolation = pkgs.haskell.lib.dontCheck (pkgs.haskellPackages.neat-interpolation_0_3_2_4.override {
  #           megaparsec = pkgs.haskellPackages.megaparsec_7_0_4;
  #         });

  pepper = pkgs.callPackage ./salt.nix {};
  cicd-shell = pkgs.haskell.lib.dontHaddock
    ( pkgs.haskellPackages.callCabal2nix
        "cicd-shell"
        (builtins.path { name = "cicd-shell"; inherit filter; path = ./.; } )
        { }
    );
  dockerTools = pkgs.dockerTools;

  exec = pkgs.haskell.lib.justStaticExecutables cicd-shell;

in

rec {

  inherit cicd-shell;

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

  shell = pkgs.mkShell {
    buildInputs = [
      exec
      pkgs.jq
      pepper
    ];
  };

  project = pkgs.buildEnv {
      name = "cicd-shell";

      paths = [
        exec
        pkgs.jq
        pepper
      ];
  };

}
