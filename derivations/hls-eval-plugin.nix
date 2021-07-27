{ mkDerivation, aeson, base, containers, deepseq, Diff, directory
, dlist, extra, filepath, ghc, ghc-boot-th, ghc-paths, ghcide
, hashable, hls-plugin-api, hls-test-utils, lens, lsp, lsp-test
, lsp-types, megaparsec, mtl, parser-combinators, pretty-simple
, QuickCheck, safe-exceptions, shake, temporary, text, time
, transformers, unliftio, unordered-containers, lib
}:
mkDerivation {
    pname = "hls-eval-plugin";
    version = "1.1.0.0";
    sha256 = "138l49a8y0g7yk29xdjs0jv0xmz3y8lvig45g944spj3xi8snpfx";
    libraryHaskellDepends = [
        aeson base containers deepseq Diff directory dlist extra filepath
        ghc ghc-boot-th ghc-paths ghcide hashable hls-plugin-api lens lsp
        lsp-types megaparsec mtl parser-combinators pretty-simple
        QuickCheck safe-exceptions shake temporary text time transformers
        unliftio unordered-containers
    ];
    testHaskellDepends = [
        aeson base directory extra filepath hls-test-utils lens lsp-test
        lsp-types text
    ];
    description = "Eval plugin for Haskell Language Server";
    license = lib.licenses.asl20;
}