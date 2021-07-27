{ mkDerivation, aeson, base, bytestring, containers, HsYAML, mtl
, scientific, text, unordered-containers, vector, lib
}:
mkDerivation {
    pname = "HsYAML-aeson";
    version = "0.2.0.0";
    sha256 = "12sxww260pc0bbpiyirm7911haxhljdi2f08a9ddpbgw8d5n7ffg";
    revision = "3";
    editedCabalFile = "0vhdndyj5f07vvvnssn5ybdja5wmaydq0n2lfpihvdg4dkhczrx2";
    libraryHaskellDepends = [
        aeson base bytestring containers HsYAML mtl scientific text
        unordered-containers vector
    ];
    description = "JSON to YAML Adapter";
    license = lib.licenses.gpl2Plus;
}