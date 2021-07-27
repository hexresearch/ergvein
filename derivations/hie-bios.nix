{ mkDerivation, aeson, base, base16-bytestring, bytestring
, conduit, conduit-extra, containers, cryptohash-sha1, deepseq
, directory, extra, file-embed, filepath, ghc, hslogger
, hspec-expectations, optparse-applicative, process, tasty
, tasty-expected-failure, tasty-hunit, temporary, text, time
, transformers, unix-compat, unordered-containers, vector, yaml, lib
}:
mkDerivation {
    pname = "hie-bios";
    version = "0.7.5";
    sha256 = "0k8g1qkxqqa0ld15s82mvkgsm4icm65dkabsv8sd2mkrhck0lfw8";
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [
        aeson base base16-bytestring bytestring conduit conduit-extra
        containers cryptohash-sha1 deepseq directory extra file-embed
        filepath ghc hslogger process temporary text time transformers
        unix-compat unordered-containers vector yaml
    ];
    executableHaskellDepends = [
        base directory filepath ghc optparse-applicative
    ];
    testHaskellDepends = [
        base directory extra filepath ghc hspec-expectations tasty
        tasty-expected-failure tasty-hunit temporary text
        unordered-containers yaml
    ];
    description = "Set up a GHC API session";
    license = lib.licenses.bsd3;
}