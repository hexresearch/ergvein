{ mkDerivation, base, bytestring, directory, filepath, ghc
, ghc-boot-th, ghcide, hls-plugin-api, hls-test-utils, lsp-types
, mtl, stylish-haskell, text, lib
}:
mkDerivation {
    pname = "hls-stylish-haskell-plugin";
    version = "1.0.0.0";
    sha256 = "1f2banm7lbl2grqrm0d9dnhk5fimxqan3xlsl4hjyqgy42xqqai2";
    libraryHaskellDepends = [
        base directory filepath ghc ghc-boot-th ghcide hls-plugin-api
        lsp-types mtl stylish-haskell text
    ];
    testHaskellDepends = [ base bytestring hls-test-utils text ];
    description = "Integration with the Stylish Haskell code formatter";
    license = lib.licenses.asl20;
}