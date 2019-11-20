{ mkDerivation, base, bytestring, QuickCheck, stdenv, tasty
, tasty-hunit, tasty-quickcheck, zlibSys, bbe
}:
mkDerivation {
  pname = "zlib";
  version = "0.6.2.1";
  sha256 = "f0f810ff173560b60392db448455c0513b3239f48e43cb494b3733aa559621d0";
  libraryHaskellDepends = [ base bytestring ];
  librarySystemDepends = [ zlibSys ];
  testHaskellDepends = [
    base bytestring QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  description = "Compression and decompression in the gzip and zlib formats";
  license = stdenv.lib.licenses.bsd3;
}
