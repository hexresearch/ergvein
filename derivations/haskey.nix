{ mkDerivation, async, base, binary, bytestring, containers
, directory, exceptions, fetchgit, filepath, focus, hashable
, haskey-btree, HUnit, list-t, lz4, mtl, QuickCheck, random
, semigroups, stdenv, stm, stm-containers, temporary
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, transformers, unix, vector, xxhash-ffi
}:
mkDerivation {
  pname = "haskey";
  version = "0.3.1.0";
  src = fetchgit {
    url = "https://github.com/hexresearch/haskey";
    sha256 = "1l32l80jhw87a5njyi83iydd1fib9x4l511s9qhfnq7447q9r4kw";
    rev = "d81cb891c845c6316bf6e6466a391fbf60bdb37f";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base binary bytestring containers directory exceptions filepath
    focus hashable haskey-btree list-t lz4 mtl semigroups stm
    stm-containers transformers unix xxhash-ffi
  ];
  testHaskellDepends = [
    async base binary bytestring containers directory exceptions
    haskey-btree HUnit mtl QuickCheck random temporary test-framework
    test-framework-hunit test-framework-quickcheck2 text transformers
    vector
  ];
  homepage = "https://github.com/haskell-haskey";
  description = "A transactional, ACID compliant, embeddable key-value store";
  license = stdenv.lib.licenses.bsd3;
}
