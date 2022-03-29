{ mkDerivation, base, binary, bitcoin-script, bitcoin-types
, bytestring, cryptohash, hexstring, hspec, lens, lib
}:
mkDerivation {
  pname = "bitcoin-tx";
  version = "0.13.1";
  sha256 = "3bb88265353066c394e96a56b2dc555fa13d37ca7f820978b793196c6829cc00";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base binary bitcoin-script bitcoin-types bytestring cryptohash
    hexstring lens
  ];
  testHaskellDepends = [
    base bitcoin-script bytestring hexstring hspec
  ];
  homepage = "http://www.leonmergen.com/opensource.html";
  description = "Utility functions for manipulating bitcoin transactions";
  license = lib.licenses.mit;
}
