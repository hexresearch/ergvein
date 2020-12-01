{ mkDerivation, base, bytestring, containers, directory, filepath
, mtl, pandoc-types, parsec, process, split, stdenv, syb, temporary
, text, utf8-string, xml
}:
mkDerivation {
  pname = "texmath";
  version = "0.11.3";
  sha256 = "2e01dd4534f3dba26a4d5ec889dcb2c6195bd0211b125eab305aae41eeea370f";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers mtl pandoc-types parsec syb xml
  ];
  testHaskellDepends = [
    base bytestring directory filepath process split temporary text
    utf8-string xml
  ];
  homepage = "http://github.com/jgm/texmath";
  description = "Conversion between formats used to represent mathematics";
  license = stdenv.lib.licenses.gpl2;
}
