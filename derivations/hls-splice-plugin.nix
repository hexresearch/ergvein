{ mkDerivation, aeson, base, containers, directory, dlist, extra
, filepath, foldl, ghc, ghc-exactprint, ghcide, hls-plugin-api
, hls-test-utils, lens, lsp, retrie, shake, syb, text, transformers
, unliftio-core, unordered-containers, lib
}:
mkDerivation {
    pname = "hls-splice-plugin";
    version = "1.0.0.1";
    sha256 = "0fyc2z1bh64plqf831f19nqkgkhryklgrrql2cn25jhfg55gf95q";
    libraryHaskellDepends = [
        aeson base containers dlist extra foldl ghc ghc-exactprint ghcide
        hls-plugin-api lens lsp retrie shake syb text transformers
        unliftio-core unordered-containers
    ];
    testHaskellDepends = [
        base directory extra filepath hls-test-utils text
    ];
    description = "HLS Plugin to expand TemplateHaskell Splices and QuasiQuotes";
    license = lib.licenses.asl20;
}