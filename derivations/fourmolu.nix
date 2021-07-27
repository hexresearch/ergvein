{ mkDerivation, aeson, base, bytestring, containers, directory
, dlist, exceptions, filepath, ghc-lib-parser, gitrev, hspec
, hspec-discover, HsYAML, HsYAML-aeson, mtl, optparse-applicative
, path, path-io, syb, text, lib
}:
mkDerivation {
    pname = "fourmolu";
    version = "0.3.0.0";
    sha256 = "0v89dvcr8l0swj23kkakc39q6lyxjz90rqgwy7m6a5p6iv3h2wms";
    revision = "1";
    editedCabalFile = "1n3avdmjqkd2910lhb5spxvjgzb7icln82pcrz3cmkfmjwxnirsc";
    isLibrary = true;
    isExecutable = true;
    enableSeparateDataOutput = true;
    libraryHaskellDepends = [
        aeson base bytestring containers directory dlist exceptions
        filepath ghc-lib-parser HsYAML HsYAML-aeson mtl syb text
    ];
    executableHaskellDepends = [
        base directory ghc-lib-parser gitrev optparse-applicative text
    ];
    testHaskellDepends = [
        base containers filepath hspec path path-io text
    ];
    testToolDepends = [ hspec-discover ];
    description = "A formatter for Haskell source code";
    license = lib.licenses.bsd3;
}