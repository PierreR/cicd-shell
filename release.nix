let
  bootstrap = import <nixpkgs> { };
  nixpkgs = builtins.fromJSON (builtins.readFile ./share/.nixpkgs.json);
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          dhall = self.callCabal2nix "dhall" (pkgs.fetchFromGitHub {
            owner  = "Gabriel439";
            repo   = "Haskell-Dhall-Library";
            rev    = "11ceab1dfeb9ed9a25dab717b4fe24ffaf7d320e";
            sha256 = "00iz2albmj3iw8sdj2idf1y4vgfjfliv7xcxbqgmb3ggp7n7wf6a";
          }) {};
          cicd-shell = self.callPackage ./. {inherit dhall;};
        };
      };
    };
  };

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { inherit config; };
in
rec {
  project = pkgs.haskellPackages.cicd-shell;
  cicd-shell = pkgs.buildEnv rec {
    name = "cicd-shell-${project.version}";
    paths = [
      project
    ];
  };
}
