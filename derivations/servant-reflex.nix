{ mkDerivation, base, bytestring, case-insensitive, containers
, data-default, exceptions, ghcjs-dom, http-api-data, http-media
, jsaddle, mtl, network-uri, reflex, reflex-dom-core, safe, servant
, servant-auth, stdenv, string-conversions, text, transformers, fetchgit
}:
mkDerivation {
  pname = "servant-reflex";
  version = "0.3.4";
  src = fetchgit {
    url = "https://github.com/imalsogreg/servant-reflex";
    rev = "9310745a99c670ec244ecdcac6577d0f365f6946";
    sha256 = "13rk2jw87ll8qjpw7zxy2aqm92dgsywn9d9sq8m6yz2n373wdihi";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers data-default exceptions
    ghcjs-dom http-api-data http-media jsaddle mtl network-uri reflex
    reflex-dom-core safe servant servant-auth string-conversions text
    transformers
  ];
  description = "servant API generator for reflex apps";
  license = stdenv.lib.licenses.bsd3;
}
