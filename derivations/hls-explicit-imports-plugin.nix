{ mkDerivation, aeson, base, containers, deepseq, ghc, ghcide
, hls-plugin-api, lsp, lsp-types, shake, text, unordered-containers
, lib
}:
mkDerivation {
    pname = "hls-explicit-imports-plugin";
    version = "1.0.0.1";
    sha256 = "1ygs667pkc3zn66yqz6ssrxifhrc906sddmcgjnkprk7zappk2nc";
    libraryHaskellDepends = [
        aeson base containers deepseq ghc ghcide hls-plugin-api lsp
        lsp-types shake text unordered-containers
    ];
    description = "Explicit imports plugin for Haskell Language Server";
    license = lib.licenses.asl20;
}