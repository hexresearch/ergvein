{ mkDerivation, base, basement, bytestring, containers
, data-default-class, fetchgit, network, socks, stdenv, tls, x509
, x509-store, x509-system, x509-validation
}:
mkDerivation {
  pname = "connection";
  version = "0.3.1";
  src = fetchgit {
    url = "https://github.com/vincenthz/hs-connection.git";
    sha256 = "19pbqg5r0ikzi9ngcxyb247ac9r4k7nfharlqypp1c6lccrnn3k9";
    rev = "8bb6062629eff93aef6324120bc9399f24050390";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base basement bytestring containers data-default-class network
    socks tls x509 x509-store x509-system x509-validation
  ];
  homepage = "https://github.com/vincenthz/hs-connection";
  description = "Simple and easy network connections API";
  license = stdenv.lib.licenses.bsd3;
}
