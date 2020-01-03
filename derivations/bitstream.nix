{ mkDerivation, base, base-unicode-symbols, bytestring, QuickCheck
, stdenv, vector
}:
mkDerivation {
  pname = "bitstream";
  version = "0.3.0.0";
  sha256 = "70af13e53f07094ebef94c2ba9a99c8482bd30e00497d86280bfa87f5ade9e89";
  libraryHaskellDepends = [
    base base-unicode-symbols bytestring vector
  ];
  testHaskellDepends = [
    base base-unicode-symbols bytestring QuickCheck vector
  ];
  homepage = "https://github.com/phonohawk/bitstream";
  description = "Fast, packed, strict and lazy bit streams with stream fusion";
  license = stdenv.lib.licenses.publicDomain;
}
