{ mkDerivation, base, hspec, hspec-discover, mtl, stdenv, text }:
mkDerivation {
  pname = "clay";
  version = "0.13.1";
  sha256 = "844e9101cc1835eb12bac50e289d00f19c24eeee12bcdebae1b633edffa328a3";
  libraryHaskellDepends = [ base mtl text ];
  testHaskellDepends = [ base hspec hspec-discover mtl text ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://fvisser.nl/clay";
  description = "CSS preprocessor as embedded Haskell";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
}
