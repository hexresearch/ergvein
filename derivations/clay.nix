{ mkDerivation, base, fetchgit, hspec, hspec-discover, mtl, stdenv
, text
}:
mkDerivation {
  pname = "clay";
  version = "0.14.0";
  src = fetchgit {
    url = "https://github.com/sebastiaanvisser/clay.git";
    sha256 = "18g00q2xmqpxpzcw4dikxjm3lkcsrvrimhksc5pkw153mkspxawj";
    rev = "1f4e38b9d66ec46b266e75e09cbd5de33fed26b1";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base mtl text ];
  testHaskellDepends = [ base hspec hspec-discover mtl text ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://fvisser.nl/clay";
  description = "CSS preprocessor as embedded Haskell";
  license = stdenv.lib.licenses.bsd3;
}