{ mkDerivation, aeson, apply-refact, base, binary, bytestring
, containers, data-default, deepseq, Diff, directory, extra
, filepath, ghc, ghc-exactprint, ghcide, hashable, hlint
, hls-plugin-api, hslogger, lens, lsp, regex-tdfa, shake, temporary
, text, transformers, unordered-containers, lib
}:
mkDerivation {
    pname = "hls-hlint-plugin";
    version = "1.0.0.2";
    sha256 = "1qi654azf4l24sc7zaimbxm7z59xfvdvn33fsa5d8y7910w17d73";
    libraryHaskellDepends = [
        aeson apply-refact base binary bytestring containers data-default
        deepseq Diff directory extra filepath ghc ghc-exactprint ghcide
        hashable hlint hls-plugin-api hslogger lens lsp regex-tdfa shake
        temporary text transformers unordered-containers
    ];
    description = "Hlint integration plugin with Haskell Language Server";
    license = lib.licenses.asl20;
}