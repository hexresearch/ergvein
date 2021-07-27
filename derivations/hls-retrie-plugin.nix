{ mkDerivation, aeson, base, containers, deepseq, directory, extra
, ghc, ghcide, hashable, hls-plugin-api, lsp, lsp-types, retrie
, safe-exceptions, shake, text, transformers, unordered-containers
, lib
}:
mkDerivation {
    pname = "hls-retrie-plugin";
    version = "1.0.0.2";
    sha256 = "0jw1q0dk5jl80wbyvi1a6vszj9x3s7d2bnsbnyycbh4zgl33agwb";
    revision = "1";
    editedCabalFile = "03r3cb93493hr4rbd8n1ip63myssfycyijg2507kcmsly39i2qkz";
    libraryHaskellDepends = [
        aeson base containers deepseq directory extra ghc ghcide hashable
        hls-plugin-api lsp lsp-types retrie safe-exceptions shake text
        transformers unordered-containers
    ];
    description = "Retrie integration plugin for Haskell Language Server";
    license = lib.licenses.asl20;
}