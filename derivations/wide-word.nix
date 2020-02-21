{ mkDerivation, base, bytestring, deepseq, fetchgit, ghc-prim
, hedgehog, primitive, QuickCheck, quickcheck-classes, semirings
, stdenv
}:
mkDerivation {
  pname = "wide-word";
  version = "0.1.1.0";
  src = fetchgit {
    url = "https://github.com/hexresearch/wide-word";
    sha256 = "02w5379djnz8gwa05is5r43w125z8zma3dj3lw2x6fk7l5c2qjjg";
    rev = "5f59e52c30f1eca652565b1a5f18ff1368587dfe";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base deepseq ghc-prim primitive ];
  testHaskellDepends = [
    base bytestring ghc-prim hedgehog primitive QuickCheck
    quickcheck-classes semirings
  ];
  homepage = "https://github.com/erikd/wide-word";
  description = "Data types for large but fixed width signed and unsigned integers";
  license = stdenv.lib.licenses.bsd2;
}
