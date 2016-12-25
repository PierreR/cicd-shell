with (import <nixpkgs> {}).pkgs;
with haskell.lib;
let
  turtle = haskellPackages.turtle_1_3_0.overrideScope (self: super: {
     optparse-applicative = self.optparse-applicative_0_13_0_0;
  });
  cicd-shell = dontCheck (dontHaddock(haskellPackages.callPackage ./. {
    inherit turtle;
  }));
in
stdenv.mkDerivation {
  name = "cicd-shell-env";
  buildInput = [cicd-shell];
}
