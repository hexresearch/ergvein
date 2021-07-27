{ mkDerivation, base, bytestring, containers, filepath, ghc
, ghc-exactprint, ghcide, hls-plugin-api, hls-test-utils, lsp-types
, text, unordered-containers, lib
}:
mkDerivation {
    pname = "hls-haddock-comments-plugin";
    version = "1.0.0.1";
    sha256 = "1qny8y52myd3ic893wxapbqhfdcdbil8acky91lfcylr9141154i";
    libraryHaskellDepends = [
        base containers ghc ghc-exactprint ghcide hls-plugin-api lsp-types
        text unordered-containers
    ];
    testHaskellDepends = [
        base bytestring filepath hls-test-utils text
    ];
    description = "Haddock comments plugin for Haskell Language Server";
    license = lib.licenses.asl20;
}