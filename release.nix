let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./share/.nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { };
  hghc = pkgs.haskellPackages;
  dhall = hghc.callCabal2nix "dhall" (pkgs.fetchFromGitHub {
    owner  = "Gabriel439";
    repo   = "Haskell-Dhall-Library";
    rev    = "505a786c6dd7dcc37e43f3cc96031d30028625be";
    sha256 = "1dsjy4czxcwh4gy7yjffzfrbb6bmnxbixf1sy8aqrbkavgmh8s29";
  }) {};
in
  {
    project0 = pkgs.haskellPackages.callPackage ./default.nix { inherit dhall; };
  }
