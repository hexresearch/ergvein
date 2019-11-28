{ mkDerivation, base, fetchgit, hspec, hspec-discover, mtl, stdenv
, text
}:
mkDerivation {
  pname = "clay";
  version = "0.14.0";
  src = fetchgit {
    url = "https://github.com/hexresearch/clay.git";
    sha256 = "01jz1aqrrrg6x5q108axa3n7yhfrd1c72gj67xkjbpj35hwja1y9";
    rev = "50572e48fc616b4aeb9f121167106e41c243025b";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base mtl text ];
  testHaskellDepends = [ base hspec hspec-discover mtl text ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://fvisser.nl/clay";
  description = "CSS preprocessor as embedded Haskell";
  license = stdenv.lib.licenses.bsd3;
}
