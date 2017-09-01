# You can build this repository using Nix by running:
#
#     $ nix-build shell.nix
#
# You can also open up this repository inside a Nix shell by running:
#
#     $ nix-shell
#
{ nixpkgs ? import ~/.config/nixpkgs/pin.nix, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;
  hlib = pkgs.haskell.lib;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  drv = hlib.overrideCabal
  ( hlib.dontHaddock (hlib.justStaticExecutables (haskellPackages.callPackage ./. {
    protolude = haskellPackages.protolude_0_2;
  })))
  ( oldDerivation: {
    executableSystemDepends = [ pkgs.bash] ;
  });

in

if pkgs.lib.inNixShell then drv.env else drv
