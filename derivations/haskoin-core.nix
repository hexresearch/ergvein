{ mkDerivation, aeson, array, base, base16-bytestring, bytestring
, cereal, conduit, containers, cryptonite, entropy, hashable, hspec
, hspec-discover, HUnit, memory, mtl, murmur3, network, QuickCheck
, safe, scientific, secp256k1-haskell, split, stdenv
, string-conversions, text, time, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "haskoin-core";
  version = "0.8.0";
  sha256 = "ef12cd5d889d1f99ded46b6c1354e82e6ba927fcad3d2b498a9852bd601f7c79";
  libraryHaskellDepends = [
    aeson array base base16-bytestring bytestring cereal conduit
    containers cryptonite entropy hashable memory mtl murmur3 network
    QuickCheck scientific secp256k1-haskell split string-conversions
    text time transformers unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring cereal containers hspec HUnit mtl QuickCheck
    safe split string-conversions text vector
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/haskoin/haskoin#readme";
  description = "Bitcoin & Bitcoin Cash library for Haskell";
  license = stdenv.lib.licenses.publicDomain;
}
