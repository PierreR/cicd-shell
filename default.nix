# You can build with nix using
#
#     $ nix-build
#
{ pkgs ? import ./share/pin.nix { } }:

(import ./release.nix { inherit pkgs; }).project