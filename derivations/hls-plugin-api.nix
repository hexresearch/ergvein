{ mkDerivation, aeson, base, containers, data-default
, dependent-map, dependent-sum, Diff, dlist, hashable, hslogger
, lens, lsp, opentelemetry, process, regex-tdfa, shake, text, unix
, unordered-containers, lib
}:
mkDerivation {
    pname = "hls-plugin-api";
    version = "1.1.0.0";
    sha256 = "1i18a5gxa409882zpviy4paldaq43j4z1zmgr9mag2mn64vplrxy";
    libraryHaskellDepends = [
        aeson base containers data-default dependent-map dependent-sum Diff
        dlist hashable hslogger lens lsp opentelemetry process regex-tdfa
        shake text unix unordered-containers
    ];
    description = "Haskell Language Server API for plugin communication";
    license = lib.licenses.asl20;
}