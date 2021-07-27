{ mkDerivation, array, base, bytestring, containers, directory
, filepath, ghc, ghc-boot, transformers, lib
}:
mkDerivation {
    pname = "hie-compat";
    version = "0.1.0.0";
    sha256 = "00wnb7ylahglw167n2n1b2a6b0b75ay167zzrl10jsskivxwx3h3";
    libraryHaskellDepends = [
        array base bytestring containers directory filepath ghc ghc-boot
        transformers
    ];
    description = "HIE files for GHC 8.6 and other HIE file backports";
    license = lib.licenses.asl20;
}