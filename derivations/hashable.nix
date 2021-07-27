{ mkDerivation, base, bytestring, criterion, deepseq, ghc-prim
, HUnit, integer-gmp, QuickCheck, random, siphash, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, unix, lib
}:
mkDerivation {
    pname = "hashable";
    version = "1.3.0.0";
    sha256 = "1d4sn4xjf0swrfg8pl93ipavbj12ch3a9aykhkl6mjnczc9m8bl2";
    revision = "2";
    editedCabalFile = "16va8hx4ynw0n5s2warhs13ilj7hrs5fcdn140h1fiix480as36n";
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [
        base bytestring deepseq ghc-prim integer-gmp text
    ];
    testHaskellDepends = [
        base bytestring ghc-prim HUnit QuickCheck random test-framework
        test-framework-hunit test-framework-quickcheck2 text unix
    ];
    benchmarkHaskellDepends = [
        base bytestring criterion ghc-prim integer-gmp siphash text
    ];
    description = "A class for types that can be converted to a hash value";
    license = lib.licenses.bsd3;
}