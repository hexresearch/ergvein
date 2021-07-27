{ mkDerivation, algebraic-graphs, ansi-terminal, array, base
, bytestring, containers, directory, extra, filepath, ghc
, ghc-paths, hie-compat, hspec, lucid, mtl, optparse-applicative
, process, sqlite-simple, temporary, text, lib
}:
mkDerivation {
    pname = "hiedb";
    version = "0.3.0.1";
    sha256 = "1ci68q5r42rarmj12vrmggnj7c7jb8sw3wnmzrq2gn7vqhrr05jc";
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [
        algebraic-graphs ansi-terminal array base bytestring containers
        directory extra filepath ghc hie-compat lucid mtl
        optparse-applicative sqlite-simple text
    ];
    executableHaskellDepends = [ base ghc-paths ];
    testHaskellDepends = [
        base directory filepath ghc ghc-paths hspec process temporary
    ];
    description = "Generates a references DB from .hie files";
    license = lib.licenses.bsd3;
}