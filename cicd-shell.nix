{ mkDerivation, aeson, ascii-progress, async, base
, concurrent-output, data-default-class, dhall, directory, filepath
, haskeline, lens, neat-interpolation, optparse-applicative
, prettyprinter, prettyprinter-ansi-terminal, process, protolude
, raw-strings-qq, req, stdenv, tasty, tasty-discover, tasty-golden
, tasty-hunit, text, transformers, turtle
}:
mkDerivation {
  pname = "cicd-shell";
  version = "2.4.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base concurrent-output data-default-class dhall directory
    filepath haskeline lens neat-interpolation optparse-applicative
    prettyprinter prettyprinter-ansi-terminal process protolude
    raw-strings-qq req text transformers
  ];
  executableHaskellDepends = [
    ascii-progress async base directory lens text turtle
  ];
  testHaskellDepends = [
    base filepath tasty tasty-discover tasty-golden tasty-hunit text
  ];
  homepage = "ssh://git@stash.cirb.lan:7999/cicd/salt-shell.git";
  description = "Command line tool that interfaces cicd services";
  license = stdenv.lib.licenses.bsd3;
}
