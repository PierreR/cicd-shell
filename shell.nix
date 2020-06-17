# This is the developer shell which is useful to develop the cicd shell
let
  pkgs = import ./share/pin.nix {};
  cicd-shell = (import ./release.nix { inherit pkgs; }).cicd-shell;
in
pkgs.haskellPackages.shellFor {
  buildInputs = with pkgs; [
    zlib.dev
    zlib.out
    cabal-install
  ];

  packages = (hp: [ cicd-shell]);

}
