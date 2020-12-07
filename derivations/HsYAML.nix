{ mkDerivation, base, bytestring, containers, mtl, parsec, stdenv
, text
}:
mkDerivation {
  pname = "HsYAML";
  version = "0.1.1.3";
  sha256 = "5dd03c26c4d82e9cebab4ba6104389790aa0dbd411eafcd56245c7b65ae5932b";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers mtl parsec text
  ];
  homepage = "https://github.com/hvr/HsYAML";
  description = "Pure Haskell YAML 1.2 parser";
  license = stdenv.lib.licenses.gpl2;
}
