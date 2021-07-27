{ mkDerivation, aeson, base, containers, deepseq, directory, extra
, filepath, fingertree, generic-lens, ghc, ghc-boot-th
, ghc-exactprint, ghc-source-gen, ghcide, hls-plugin-api
, hls-test-utils, hspec, hspec-discover, hspec-expectations, lens
, lsp, lsp-types, mtl, QuickCheck, refinery, retrie, shake, syb
, tasty-hspec, tasty-hunit, text, transformers
, unordered-containers, lib
}:
mkDerivation {
    pname = "hls-tactics-plugin";
    version = "1.1.0.0";
    sha256 = "13qysl6dwrn15kn310r04g1yv7jj9xhar659lrc8h230c4khn2qv";
    libraryHaskellDepends = [
        aeson base containers deepseq directory extra filepath fingertree
        generic-lens ghc ghc-boot-th ghc-exactprint ghc-source-gen ghcide
        hls-plugin-api lens lsp mtl refinery retrie shake syb text
        transformers unordered-containers
    ];
    testHaskellDepends = [
        aeson base containers deepseq directory filepath ghc ghcide
        hls-plugin-api hls-test-utils hspec hspec-expectations lens
        lsp-types mtl QuickCheck tasty-hspec tasty-hunit text
    ];
    testToolDepends = [ hspec-discover ];
    description = "Wingman plugin for Haskell Language Server";
    license = lib.licenses.asl20;
}