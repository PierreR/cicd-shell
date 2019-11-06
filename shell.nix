# This is the developer shell which is useful to develop the cicd shell
let
  pkgs = import ./share/pin.nix { };
  ghcEnv = pkgs.haskellPackages.ghcWithPackages (hpkgs: (import ./release.nix {inherit pkgs;}).cicd-shell.propagatedBuildInputs);
in pkgs.mkShell {
  buildInputs = [
    pkgs.zlib.dev
    pkgs.zlib.out
    ghcEnv
    ];
}