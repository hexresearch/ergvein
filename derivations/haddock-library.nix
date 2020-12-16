{ mkDerivation, base, base-compat, bytestring, containers, deepseq
, hspec, hspec-discover, parsec, QuickCheck, stdenv, text
, transformers
}:
mkDerivation {
  pname = "haddock-library";
  version = "1.7.0";
  sha256 = "e7c2794a7eb352824f098cafa07bc0506f2c3633808bfbf4c64aef0ba664d011";
  libraryHaskellDepends = [
    base bytestring containers parsec text transformers
  ];
  testHaskellDepends = [
    base base-compat bytestring containers deepseq hspec parsec
    QuickCheck text transformers
  ];
  testToolDepends = [ hspec-discover ];
  doCheck = false;
  homepage = "http://www.haskell.org/haddock/";
  description = "Library exposing some functionality of Haddock";
  license = stdenv.lib.licenses.bsd3;
}
