{ mkDerivation, aeson, attoparsec, base, base-compat, bifunctors
, bytestring, Cabal, cabal-doctest, case-insensitive, deepseq
, doctest, hspec, hspec-discover, http-api-data, http-media
, http-types, mmorph, mtl, network-uri, QuickCheck
, quickcheck-instances, singleton-bool, stdenv, string-conversions
, tagged, text, transformers, vault
}:
mkDerivation {
  pname = "servant";
  version = "0.16";
  sha256 = "67c093fa363acb837e8f24b38c0b65340012a6de6c65df3b7d3415d22099973a";
  revision = "2";
  editedCabalFile = "1p8x7lhn969fd6vwjr031znr47ppydiwrvhjj06rymdz4avznhyl";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bifunctors bytestring
    case-insensitive deepseq http-api-data http-media http-types mmorph
    mtl network-uri QuickCheck singleton-bool string-conversions tagged
    text transformers vault
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring doctest hspec mtl QuickCheck
    quickcheck-instances string-conversions text transformers
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://docs.servant.dev/";
  description = "A family of combinators for defining webservices APIs";
  license = stdenv.lib.licenses.bsd3;
}
