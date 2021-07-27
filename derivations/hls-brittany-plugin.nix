{ mkDerivation, base, brittany, bytestring, filepath, ghc
, ghc-boot-th, ghcide, hls-plugin-api, hls-test-utils, lens
, lsp-types, text, transformers, lib
}:
mkDerivation {
    pname = "hls-brittany-plugin";
    version = "1.0.0.1";
    sha256 = "1lfhgvxs0bvs67raxalvj8pr4qln1yvi7i7wlp33gpk2x89bwaqy";
    libraryHaskellDepends = [
        base brittany filepath ghc ghc-boot-th ghcide hls-plugin-api lens
        lsp-types text transformers
    ];
    testHaskellDepends = [ base bytestring hls-test-utils text ];
    description = "Integration with the Brittany code formatter";
    license = lib.licenses.asl20;
}