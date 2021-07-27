{ mkDerivation, base, containers, directory, filepath, ghc
, ghc-paths, process, safe-exceptions, template-haskell, th-compat
, transformers, lib
}:
mkDerivation {
    pname = "ghc-check";
    version = "0.5.0.4";
    sha256 = "05yrj2xm3b44h2c5r5qxsfwm1v89zhv0l30wdcc6439hd94w1w8q";
    libraryHaskellDepends = [
        base containers directory filepath ghc ghc-paths process
        safe-exceptions template-haskell th-compat transformers
    ];
    description = "detect mismatches between compile-time and run-time versions of the ghc api";
    license = lib.licenses.bsd3;
}