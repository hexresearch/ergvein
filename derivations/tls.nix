{ mkDerivation, asn1-encoding, asn1-types, async, base, bytestring
, cereal, cryptonite, data-default-class, fetchgit, gauge
, hourglass, memory, mtl, network, QuickCheck, stdenv, tasty
, tasty-quickcheck, transformers, x509, x509-store, x509-validation
}:
mkDerivation {
  pname = "tls";
  version = "1.5.4";
  src = fetchgit {
    url = "https://github.com/vincenthz/hs-tls";
    sha256 = "1hfkdlczb7y1xw3fw26q9s4gla8gxr7f87b1qsr7hrdw2gl4isyi";
    rev = "f01216dffaa50def08953fe5c0a206eb28427a49";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    asn1-encoding asn1-types async base bytestring cereal cryptonite
    data-default-class hourglass memory mtl network transformers x509
    x509-store x509-validation
  ];
  testHaskellDepends = [
    asn1-types async base bytestring cryptonite data-default-class
    hourglass QuickCheck tasty tasty-quickcheck x509 x509-validation
  ];
  benchmarkHaskellDepends = [
    asn1-types async base bytestring cryptonite data-default-class
    gauge hourglass QuickCheck tasty-quickcheck x509 x509-validation
  ];
  homepage = "http://github.com/vincenthz/hs-tls";
  description = "TLS/SSL protocol native implementation (Server and Client)";
  license = stdenv.lib.licenses.bsd3;
}

