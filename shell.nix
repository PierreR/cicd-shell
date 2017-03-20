# You can build this repository using Nix by running:
#
#     $ nix-build shell.nix
#
# You can also open up this repository inside a Nix shell by running:
#
#     $ nix-shell
#
{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;
  hlib = pkgs.haskell.lib;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  drv = hlib.dontHaddock(haskellPackages.callPackage ./. {dhall = haskellPackages.dhall_git;});

in

if pkgs.lib.inNixShell then drv.env else drv
