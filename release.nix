let
  pkgs = import <nixpkgs> { };
  turtle = pkgs.haskellPackages.turtle_1_3_0.overrideScope (self: super: {
    optparse-applicative = self.optparse-applicative_0_13_0_0;
  });
in
  {
    project0 = pkgs.haskellPackages.callPackage ./default.nix { inherit turtle; };
  }
