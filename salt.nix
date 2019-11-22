/*
0.6.0 is the last version of 'pepper' that works with our current salt master version (2016.11.5)
*/
{ lib
, python2Packages
, salt
}:

python2Packages.buildPythonApplication rec {
  pname = "salt-pepper";
  version = "0.6.0";
  src = python2Packages.fetchPypi {
    inherit pname version;
    sha256 = "161x84m0w56i7s1mqgkg1ysrb6avw0d2bw49z3an19p338ixn50p";
  };

  buildInputs = with python2Packages; [ setuptools setuptools_scm salt ];
  checkInputs = with python2Packages; [
    pytest
    mock
    pyzmq
    pytest-rerunfailures
    pytestcov
    cherrypy
    tornado_4
  ];

  meta = with lib; {
    description = "A CLI front-end to a running salt-api system";
    homepage = https://github.com/saltstack/pepper;
    maintainers = [ maintainers.pierrer ];
    license = licenses.asl20;
  };
}
