{ mkDerivation, base, lens, optional-args, process
, raw-strings-qq, stdenv, text, turtle
}:
mkDerivation {
  pname = "cicd-shell";
  version = "0.9.10";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base lens optional-args process raw-strings-qq text turtle
  ];
  homepage = "ssh://git@stash.cirb.lan:7999/cicd/salt-shell.git";
  license = stdenv.lib.licenses.bsd3;
}
