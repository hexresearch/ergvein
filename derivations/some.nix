{ mkDerivation, base, deepseq, lib }:
mkDerivation {
    pname = "some";
    version = "1.0.3";
    sha256 = "0w3syapwz9v916zf1i4f8vxymdfg7syc2cpxgnqr018pbswzxrk2";
    libraryHaskellDepends = [ base deepseq ];
    testHaskellDepends = [ base ];
    description = "Existential type: Some";
    license = lib.licenses.bsd3;
}