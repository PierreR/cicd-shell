{ mkDerivation, base, lens, optional-args, process
, raw-strings-qq, stdenv, text, turtle, dhall
}:
mkDerivation {
  pname = "cicd-shell";
  version = "1.0.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base lens optional-args process raw-strings-qq text turtle dhall
  ];
  homepage = "ssh://git@stash.cirb.lan:7999/cicd/cicd-shell.git";
  license = stdenv.lib.licenses.bsd3;
}
