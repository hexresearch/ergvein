{ mkDerivation, aeson, base, blaze-html, blaze-markup, bytestring
, containers, hspec, parsec, scientific, stdenv, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "doctemplates";
  version = "0.2.2.1";
  sha256 = "6b0cfb565fc7fa90d71ac56b83aedecf670678e6f1441278877fbf399e9bccbf";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base blaze-html blaze-markup bytestring containers parsec
    scientific text unordered-containers vector
  ];
  testHaskellDepends = [ aeson base hspec text ];
  homepage = "https://github.com/jgm/doctemplates#readme";
  description = "Pandoc-style document templates";
  license = stdenv.lib.licenses.bsd3;
}
