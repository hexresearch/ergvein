{ mkDerivation, base, base16-bytestring, bytestring, cereal
, deepseq, entropy, fetchgit, hashable, hpack, hspec
, hspec-discover, HUnit, secp256k1Sys, mtl, QuickCheck, stdenv
, string-conversions
}:
mkDerivation {
  pname = "secp256k1-haskell";
  version = "0.2.2";
  src = fetchgit {
    url = "https://github.com/fastpok/secp256k1-haskell.git";
    sha256 = "13dls3skwwq58vqxxid7qhbpv4mf5lg31sjpbz8vrbxz67c1v6qb";
    rev = "299fffc08252a2a6ba7413001ec5910593b05f46";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base base16-bytestring bytestring cereal deepseq entropy hashable
    QuickCheck string-conversions
  ];
  libraryPkgconfigDepends = [ secp256k1Sys ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base base16-bytestring bytestring cereal deepseq entropy hashable
    hspec HUnit mtl QuickCheck string-conversions
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "http://github.com/haskoin/secp256k1-haskell#readme";
  description = "Bindings for secp256k1 library from Bitcoin Core";
  license = stdenv.lib.licenses.publicDomain;
}
