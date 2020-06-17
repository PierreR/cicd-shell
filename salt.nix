/*
0.6.0 is the last version of 'pepper' that works with our current salt master version (2016.11.5)
*/
{ lib
, python3Packages
, salt
}:

python3Packages.buildPythonApplication rec {
  pname = "salt-pepper";
  version = "0.6.0";
  src = python3Packages.fetchPypi {
    inherit pname version;
    sha256 = "161x84m0w56i7s1mqgkg1ysrb6avw0d2bw49z3an19p338ixn50p";
  };

  propagedBuildInputs = with python3Packages; [ setuptools setuptools_scm salt ];
  checkInputs = with python3Packages; [
    pytest
    mock
    pyzmq
    pytest-rerunfailures
    pytestcov
    cherrypy
    tornado_4
  ];

}
