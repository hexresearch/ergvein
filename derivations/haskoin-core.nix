{ mkDerivation, aeson, array, base, base16-bytestring, bytestring
, cereal, conduit, containers, cryptonite, deepseq, entropy
, fetchgit, hashable, hpack, hspec, hspec-discover, HUnit, memory
, mtl, murmur3, network, QuickCheck, safe, scientific
, secp256k1-haskell, split, stdenv, string-conversions, text, time
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "haskoin-core";
  version = "0.12.0";
  src = fetchgit {
    url = "https://github.com/haskoin/haskoin-core.git";
    sha256 = "01j2ck638k8hm12rk3hqhb13gadgnjx3qas944ycbr011zm5vhzq";
    rev = "39c0e927ee9b23b9591417a3ec0b8565df3d7b7b";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson array base base16-bytestring bytestring cereal conduit
    containers cryptonite deepseq entropy hashable hspec HUnit memory
    mtl murmur3 network QuickCheck safe scientific secp256k1-haskell
    split string-conversions text time transformers
    unordered-containers vector
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base bytestring cereal containers deepseq hspec HUnit mtl
    QuickCheck safe split string-conversions text unordered-containers
    vector
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "http://github.com/haskoin/haskoin#readme";
  description = "Bitcoin & Bitcoin Cash library for Haskell";
  license = stdenv.lib.licenses.publicDomain;
}