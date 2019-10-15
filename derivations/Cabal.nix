{ mkDerivation, array, base, base-compat, base-orphans, binary
, bytestring, containers, deepseq, Diff, directory, filepath
, integer-logarithms, mtl, optparse-applicative, parsec, pretty
, process, QuickCheck, stdenv, tagged, tar, tasty, tasty-golden
, tasty-hunit, tasty-quickcheck, temporary, text, time
, transformers, tree-diff, unix
}:
mkDerivation {
  pname = "Cabal";
  version = "2.4.1.0";
  sha256 = "736a902da9fb2c826e75e9f7b4b591983bf58a6a62c8cae9866f6a9d5ace3594";
  revision = "2";
  editedCabalFile = "04kg5xh8yabmp1ymk32gw2r66l76338rsglq8i4j2913bhq23vwa";
  setupHaskellDepends = [ mtl parsec ];
  libraryHaskellDepends = [
    array base binary bytestring containers deepseq directory filepath
    mtl parsec pretty process text time transformers unix
  ];
  testHaskellDepends = [
    array base base-compat base-orphans bytestring containers deepseq
    Diff directory filepath integer-logarithms optparse-applicative
    pretty process QuickCheck tagged tar tasty tasty-golden tasty-hunit
    tasty-quickcheck temporary text tree-diff
  ];
  doCheck = false;
  homepage = "http://www.haskell.org/cabal/";
  description = "A framework for packaging Haskell software";
  license = stdenv.lib.licenses.bsd3;
}
