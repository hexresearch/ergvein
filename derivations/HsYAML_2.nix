{ mkDerivation, base, bytestring, containers, deepseq, mtl, parsec
, QuickCheck, tasty, tasty-quickcheck, text, lib
}:
mkDerivation {
    pname = "HsYAML";
    version = "0.2.1.0";
    sha256 = "10qzhsg789h37q22hm9p27dx4rhbykcbxp7p3pvkws8fr7ajgxv0";
    revision = "2";
    editedCabalFile = "0f7867jfzlmlqnkv3fjrzjvvfzjlvhbm10kmg7n0qk69ic8grkbc";
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [
    base bytestring containers deepseq mtl parsec text
    ];
    testHaskellDepends = [
        base bytestring containers mtl QuickCheck tasty tasty-quickcheck
        text
    ];
    description = "Pure Haskell YAML 1.2 processor";
    license = lib.licenses.gpl2Only;
}