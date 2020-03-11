{ mkDerivation, aeson, base, base58string, binary, bitcoin-block
, bitcoin-script, bitcoin-tx, bitcoin-types, bytestring, exceptions
, fetchgit, hexstring, hspec, http-client, http-types, lens
, lens-aeson, stdenv, text, unordered-containers, wreq
}:
mkDerivation {
  pname = "bitcoin-api";
  version = "0.12.1";
  doCheck = false;
  src = fetchgit {
    url = "https://github.com/hexresearch/haskell-bitcoin-api";
    sha256 = "1jv19adzi5kmb6a6i9ifxb32c9dnyy6nh1qwwpzmmzxd23xc8xzl";
    rev = "eeafca3112c072f9ff06338fc010ae2229efb6b1";
    fetchSubmodules = true;
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base base58string binary bitcoin-block bitcoin-script
    bitcoin-tx bitcoin-types bytestring exceptions hexstring
    http-client http-types lens lens-aeson text unordered-containers
    wreq
  ];
  testHaskellDepends = [
    base base58string bitcoin-script bitcoin-tx bytestring hspec
    http-client lens text wreq
  ];
  homepage = "http://www.leonmergen.com/opensource.html";
  description = "Provides access to the RPC API of Bitcoin Core";
  license = stdenv.lib.licenses.mit;
}
