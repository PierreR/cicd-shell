# This is the developer shell which is useful to develop the cicd shell
{ pkgs ? import <nixpkgs> { } }:
let
  cicd-shell = (import ./release.nix { inherit pkgs; }).cicd-shell;
in
pkgs.haskellPackages.shellFor {
  buildInputs = with pkgs; [
    zlib.dev
    zlib.out
    cabal-install
    nixpkgs-fmt
  ];

  packages = (hp: [ cicd-shell]);

}
