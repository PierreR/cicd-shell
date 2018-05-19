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
      in
      {
        haskellPackages = super.haskellPackages.override {
          overrides = hself: hsuper: rec {
            project = hlib.overrideCabal
              ( hsuper.callPackage ./. { })
              ( csuper: { executableSystemDepends = [ self.jq self.pepper ];});
        };
      };
  };
  pkgs = import ./share/pin.nix { overlays = [ hoverlays];};
  dockerTools = pkgs.dockerTools;
in
rec {
  project =  pkgs.haskell.lib.justStaticExecutables ( pkgs.haskellPackages.project );

  docker = dockerTools.buildImage {
    name = "pi3r/cicd-shell";
    fromImage = dockerTools.pullImage {
      imageName = "nixos/nix";
      imageDigest = "sha256:d414c533649e405f2a5966b5019f042c84e511cf468965c9ef03719ae2b0c124";
      sha256 = "045swkdqjh24pgb8kb798ya34my5ryvbikzavxi3ns8y7l0prxff";
      finalImageTag = "2.0";
    };
    config.Entrypoint = [ "${pkgs.bash}/bin/bash"];
    # config.Entrypoint = [ "${project}/bin/cicd"];
    config.Env = [
      "PATH=/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/bin:/sbin:/usr/bin:/usr/sbin:${project}/bin:${pkgs.jq}/bin:${pkgs.pepper}/bin"
    ];
  };
}
