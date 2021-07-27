{ mkDerivation, ansi-terminal, async, base, bytestring, containers
, data-default, deepseq, directory, filepath, ghc, ghc-exactprint
, ghc-paths, haskell-src-exts, HUnit, mtl, optparse-applicative
, process, random-shuffle, syb, tasty, tasty-hunit, temporary, text
, transformers, unordered-containers, lib
}:
mkDerivation {
    pname = "retrie";
    version = "1.0.0.0";
    sha256 = "1hrkx7gx7hwgljvx8zm1yhn7xvyif943hvxx5b27ali6grmndy9w";
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [
        ansi-terminal async base bytestring containers data-default
        directory filepath ghc ghc-exactprint mtl optparse-applicative
        process random-shuffle syb text transformers unordered-containers
    ];
    executableHaskellDepends = [ base haskell-src-exts ];
    testHaskellDepends = [
        base containers data-default deepseq directory filepath ghc
        ghc-paths haskell-src-exts HUnit mtl optparse-applicative process
        syb tasty tasty-hunit temporary text unordered-containers
    ];
    description = "A powerful, easy-to-use codemodding tool for Haskell";
    license = lib.licenses.mit;
}