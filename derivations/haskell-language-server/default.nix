{ mkDerivation, aeson, aeson-pretty, async, base
, base16-bytestring, binary, bytestring, containers
, cryptohash-sha1, data-default, deepseq, directory, extra
, filepath, floskell, fourmolu, fuzzy, ghc, ghc-boot-th, ghc-paths
, ghcide, gitrev, hashable, hie-bios, hiedb, hls-brittany-plugin
, hls-class-plugin, hls-eval-plugin, hls-explicit-imports-plugin
, hls-haddock-comments-plugin, hls-hlint-plugin, hls-plugin-api
, hls-retrie-plugin, hls-splice-plugin, hls-stylish-haskell-plugin
, hls-tactics-plugin, hls-test-utils, hslogger, hspec-expectations
, lens, lsp, lsp-test, lsp-types, mtl, optparse-applicative
, optparse-simple, ormolu, process, regex-tdfa, safe-exceptions
, shake, sqlite-simple, temporary, text, transformers
, unordered-containers, lib
}:
mkDerivation {
pname = "haskell-language-server";
version = "1.1.0.0";
sha256 = "0zbr8qr097mjcsbgdkm6a8ala1ifdajmllr8basvrndn28zgi5cg";
isLibrary = true;
isExecutable = true;
libraryHaskellDepends = [
    aeson-pretty async base base16-bytestring bytestring containers
    cryptohash-sha1 data-default directory extra filepath ghc ghcide
    gitrev hie-bios hiedb hls-plugin-api hslogger lsp
    optparse-applicative optparse-simple process safe-exceptions shake
    sqlite-simple text unordered-containers
];
executableHaskellDepends = [
    aeson async base base16-bytestring binary bytestring containers
    cryptohash-sha1 data-default deepseq directory extra filepath
    floskell fourmolu fuzzy ghc ghc-boot-th ghc-paths ghcide gitrev
    hashable hie-bios hiedb hls-brittany-plugin hls-class-plugin
    hls-eval-plugin hls-explicit-imports-plugin
    hls-haddock-comments-plugin hls-hlint-plugin hls-plugin-api
    hls-retrie-plugin hls-splice-plugin hls-stylish-haskell-plugin
    hls-tactics-plugin hslogger lens lsp mtl optparse-applicative
    optparse-simple ormolu process regex-tdfa safe-exceptions shake
    sqlite-simple temporary text transformers unordered-containers
];
testHaskellDepends = [
    aeson base bytestring containers data-default directory extra
    filepath ghcide hls-plugin-api hls-test-utils hspec-expectations
    lens lsp-test lsp-types process text unordered-containers
];
testToolDepends = [ ghcide ];
description = "LSP server for GHC";
license = lib.licenses.asl20;
maintainers = with lib.maintainers; [ maralorn ];
}