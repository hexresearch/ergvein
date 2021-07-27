{ mkDerivation, aeson, base, bytestring, containers, filepath, ghc
, ghc-exactprint, ghcide, hls-plugin-api, hls-test-utils, lens, lsp
, lsp-test, lsp-types, shake, text, transformers
, unordered-containers, lib
}:
mkDerivation {
    pname = "hls-class-plugin";
    version = "1.0.0.1";
    sha256 = "0s9pkdcgvfb9qhj9qjy6bygdyshanczcn9apq3qcw8yznl1zqilc";
    libraryHaskellDepends = [
        aeson base containers ghc ghc-exactprint ghcide hls-plugin-api lens
        lsp shake text transformers unordered-containers
    ];
    testHaskellDepends = [
        base bytestring filepath hls-test-utils lens lsp-test lsp-types
        text
    ];
    description = "Class/instance management plugin for Haskell Language Server";
    license = lib.licenses.asl20;
}