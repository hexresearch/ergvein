{ mkDerivation, base, primitive, primitive-unaligned, stdenv }:
mkDerivation {
  pname = "byte-order";
  version = "0.1.2.0";
  sha256 = "bc103be34d25e70071a6bc1a65a7b42b9f078d2601e6ee590f66cf8a2b26d8da";
  libraryHaskellDepends = [ base primitive primitive-unaligned ];
  testHaskellDepends = [ base primitive ];
  homepage = "https://github.com/andrewthad/byte-order";
  description = "Portable big-endian and little-endian conversions";
  license = stdenv.lib.licenses.bsd3;
}
