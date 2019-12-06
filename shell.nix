# This is the developer shell which is useful to develop the cicd shell
let
  pkgs = import ./share/pin.nix {};
  ghcEnv = pkgs.haskellPackages.ghcWithPackages (hpkgs: (import ./release.nix { inherit pkgs; }).cicd-shell.propagatedBuildInputs);
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    zlib.dev
    zlib.out
    gitAndTools.pre-commit
    nixpkgs-fmt
    ghcEnv
  ];
  shellHook = ''
    export NIX_GHC='${ghcEnv}'
    export NIX_GHC_LIBDIR='${ghcEnv}/lib/ghc-'$(ghc --numeric-version)
  '';

}
