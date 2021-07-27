{ mkDerivation, base, base-compat, hspec, hspec-discover, mtl
, template-haskell, lib
}:
mkDerivation {
    pname = "th-compat";
    version = "0.1.2";
    sha256 = "009qc0yy5iq61kgnp9n6vdlqh8zmk4bjawcvpigccgfyk40mvi1b";
    libraryHaskellDepends = [ base template-haskell ];
    testHaskellDepends = [
        base base-compat hspec mtl template-haskell
    ];
    testToolDepends = [ hspec-discover ];
    description = "Backward- (and forward-)compatible Quote and Code types";
    license = lib.licenses.bsd3;
}