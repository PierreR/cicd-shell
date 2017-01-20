{ mkDerivation, base, jq, lens, optional-args, pepper, process
, raw-strings-qq, stdenv, text, turtle
}:
mkDerivation {
  pname = "cicd-shell";
  version = "0.9.7";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base lens optional-args process raw-strings-qq text turtle
  ];
  executableSystemDepends = [ jq pepper ];
  homepage = "ssh://git@stash.cirb.lan:7999/cicd/salt-shell.git";
  license = stdenv.lib.licenses.bsd3;
}
