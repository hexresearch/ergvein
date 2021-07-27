{ mkDerivation, base, base16-bytestring, bytestring, containers
, directory, extra, filepath, hie-bios, hslogger, implicit-hie
, process, temporary, text, time, transformers, unix-compat
, unordered-containers, vector, yaml, lib
}:
mkDerivation {
    pname = "implicit-hie-cradle";
    version = "0.3.0.2";
    sha256 = "185pisgqp95zkpcksqiwiyghmg01cvfa7g6wzc31004mwwb114ih";
    libraryHaskellDepends = [
        base base16-bytestring bytestring containers directory extra
        filepath hie-bios hslogger implicit-hie process temporary text time
        transformers unix-compat unordered-containers vector yaml
    ];
    testHaskellDepends = [ base ];
    description = "Auto generate hie-bios cradles";
    license = lib.licenses.bsd3;
}