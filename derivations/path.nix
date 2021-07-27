{ mkDerivation, aeson, base, bytestring, deepseq, exceptions
, filepath, genvalidity, genvalidity-hspec, genvalidity-property
, hashable, hspec, mtl, QuickCheck, template-haskell, text
, validity, lib
}:
mkDerivation {
    pname = "path";
    version = "0.7.0";
    sha256 = "1dl7yjmkcdm3wlbj1s5qvkl31apl3dnwz5jc8h3hdq0w722x4a5k";
    revision = "1";
    editedCabalFile = "0ph5qs50lm8ac58v8df0mmivqfilb1wz14568q06aws6gwj9qqpi";
    libraryHaskellDepends = [
        aeson base deepseq exceptions filepath hashable template-haskell
        text
    ];
    testHaskellDepends = [
        aeson base bytestring filepath genvalidity genvalidity-hspec
        genvalidity-property hspec mtl QuickCheck validity
    ];
    description = "Support for well-typed paths";
    license = lib.licenses.bsd3;
}