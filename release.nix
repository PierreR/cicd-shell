let
  pkgs = import <nixpkgs> {};
  pinned = import ~/.config/nixpkgs/pin.nix;
  hlib = pkgs.haskell.lib;
  haskellPackages = pinned.haskellPackages;
in
rec {
  project = hlib.dontHaddock
    ( hlib.justStaticExecutables
      ( haskellPackages.callPackage ./. {
          protolude = haskellPackages.protolude_0_2;
        }
      )
    );

  docker = pkgs.dockerTools.buildImage {
    name = "pi3r/cicd-shell";
    runAsRoot = ''
      #!${pkgs.stdenv.shell}
      mkdir /bin /tmp
      ln -s ${pkgs.bash}/bin/sh /bin/sh
    '';
    # config.Entrypoint = [ "${self.bash}/bin/bash"];
    config.Entrypoint = [ "${project}/bin/cicd"];
    config.Env = [
      "PATH=${pkgs.bash}/bin:${pkgs.iputils.out}/bin:${pkgs.jq}/bin:${pkgs.pinned}/bin:${pkgs.coreutils}/bin"
    ];
  };
}
