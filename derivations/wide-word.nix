{ mkDerivation, base, bytestring, deepseq, fetchgit, ghc-prim
, hedgehog, primitive, QuickCheck, quickcheck-classes, semirings
, stdenv
}:
mkDerivation {
  pname = "wide-word";
  version = "0.1.1.0";
  src = fetchgit {
    url = "https://github.com/erikd/wide-word.git";
    sha256 = "058ig4z0xs3i86zh83ls0zyylndgylywwb96b8h2vw7ksjys06qs";
    rev = "5179788115f4d5ae8e02a8869d0c0c1ac09e83d5";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base deepseq primitive ];
  testHaskellDepends = [
    base bytestring ghc-prim hedgehog primitive QuickCheck
    quickcheck-classes semirings
  ];
  homepage = "https://github.com/erikd/wide-word";
  description = "Data types for large but fixed width signed and unsigned integers";
  license = stdenv.lib.licenses.bsd2;
}
