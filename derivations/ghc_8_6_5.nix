{ mkDerivation, array, base, binary, bytestring, containers
, deepseq, directory, exceptions, filepath, ghc-boot, ghc-boot-th, ghc-heap
, ghci, hpc, lib, parsec, process, template-haskell, terminfo, time
, transformers, unbuildable, unix
}:
mkDerivation {
    pname = "ghc";
    version = "8.6.5";
    sha256 = "40dc5bc1088760123ef0738e44511ab8d4f379ee6d95c601d865c50b883b508f";
    libraryHaskellDepends = [
        array base binary bytestring containers deepseq directory filepath
        ghc-boot ghc-boot-th ghc-heap ghci hpc process template-haskell
        terminfo time transformers unbuildable unix
    ];
    description = "The GHC API";
    license = lib.licenses.bsd3;
    hydraPlatforms = lib.platforms.none;
    broken = true;
}