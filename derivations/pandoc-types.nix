{ mkDerivation, aeson, base, bytestring, containers, criterion
, deepseq, ghc-prim, HUnit, QuickCheck, stdenv, string-qq, syb
, test-framework, test-framework-hunit, test-framework-quickcheck2
, transformers
}:
mkDerivation {
  pname = "pandoc-types";
  version = "1.17.6.1";
  sha256 = "1834c2064918a21d346f399c4abcd424617a9a4f0f1e1a8e8ab9b590127edeb4";
  libraryHaskellDepends = [
    aeson base bytestring containers deepseq ghc-prim QuickCheck syb
    transformers
  ];
  doCheck = false;
  testHaskellDepends = [
    aeson base bytestring containers HUnit QuickCheck string-qq syb
    test-framework test-framework-hunit test-framework-quickcheck2
  ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://pandoc.org/";
  description = "Types for representing a structured document";
  license = stdenv.lib.licenses.bsd3;
}
