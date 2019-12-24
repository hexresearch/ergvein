{ mkDerivation, base, base-unicode-symbols, bytestring, fetchgit
, QuickCheck, stdenv, vector
}:
mkDerivation {
  pname = "bitstream";
  version = "0.3.0.0";
  src = fetchgit {
    url = "https://github.com/hexresearch/bitstream";
    sha256 = "1c797rms5lbjvplmnnfp8j4n3msh9c46mjzjxbh7xjlhbkq9qkai";
    rev = "2e5f54e8d304269aa6c2801e71457ae3d7c7bc3d";
    fetchSubmodules = true;
  };
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
