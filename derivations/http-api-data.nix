{ mkDerivation, attoparsec, attoparsec-iso8601, base, base-compat
, bytestring, containers, cookie, hashable, hspec, hspec-discover
, http-types, HUnit, nats, QuickCheck, quickcheck-instances, stdenv
, tagged, text, time-compat, unordered-containers, uuid-types
}:
mkDerivation {
  pname = "http-api-data";
  version = "0.4.1.1";
  sha256 = "d0aa13e2878e4b92edf71391e775476e7d36a4b5736bd5c701373002b7a823e9";
  libraryHaskellDepends = [
    attoparsec attoparsec-iso8601 base base-compat bytestring
    containers cookie hashable http-types tagged text time-compat
    unordered-containers uuid-types
  ];
  testHaskellDepends = [
    base base-compat bytestring cookie hspec HUnit nats QuickCheck
    quickcheck-instances text time-compat unordered-containers
    uuid-types
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/fizruk/http-api-data";
  description = "Converting to/from HTTP API data like URL pieces, headers and query parameters";
  license = stdenv.lib.licenses.bsd3;
}
