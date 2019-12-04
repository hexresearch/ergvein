{ mkDerivation, base, base16-bytestring, bytestring, cereal
, entropy, hashable, hspec, hspec-discover, HUnit, mtl, QuickCheck
, secp256k1-2017-12-18, stdenv, string-conversions
}:
mkDerivation {
  pname = "secp256k1-haskell";
  version = "0.1.5";
  sha256 = "cffd77a875fd4716d7ad914f51c92cb89dabd06c23cc413f329fd97f664b28e9";
  libraryHaskellDepends = [
    base base16-bytestring bytestring cereal entropy hashable
    QuickCheck string-conversions
  ];
  librarySystemDepends = [ secp256k1-2017-12-18 ];
  testHaskellDepends = [
    base base16-bytestring bytestring cereal entropy hashable hspec
    HUnit mtl QuickCheck string-conversions
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/haskoin/secp256k1-haskell#readme";
  description = "Bindings for secp256k1 library from Bitcoin Core";
  license = stdenv.lib.licenses.publicDomain;
}
