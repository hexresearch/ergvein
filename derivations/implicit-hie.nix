{ mkDerivation, attoparsec, base, directory, filepath, filepattern
, hspec, hspec-attoparsec, text, transformers, yaml, lib
}:
mkDerivation {
    pname = "implicit-hie";
    version = "0.1.2.5";
    sha256 = "0jgnsbd38gw20h1lv39bh3n9bhawkjq90ajalgjyy5pih434mphf";
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [
        attoparsec base directory filepath filepattern text transformers
        yaml
    ];
    executableHaskellDepends = [
        attoparsec base directory filepath filepattern text transformers
        yaml
    ];
    testHaskellDepends = [
        attoparsec base directory filepath filepattern hspec
        hspec-attoparsec text transformers yaml
    ];
    description = "Auto generate hie-bios cradles & hie.yaml";
    license = lib.licenses.bsd3;
}