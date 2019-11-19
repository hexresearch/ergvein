{ mkDerivation, aeson, base, base-compat, bytestring, containers
, deepseq, entropy, exceptions, hspec, hspec-discover
, http-api-data, http-client, http-media, http-types, HUnit
, kan-extensions, markdown-unlit, monad-control, mtl, network
, QuickCheck, semigroupoids, servant, servant-client-core
, servant-server, stdenv, stm, tdigest, text, time, transformers
, transformers-base, transformers-compat, wai, warp
}:
mkDerivation {
  pname = "servant-client";
  version = "0.16";
  sha256 = "b2bbd875d6d13d6575782e5ea9d3a1511e19102e8f3d76cb01a54ebc29768118";
  revision = "4";
  editedCabalFile = "0fa37fdas1dsgd6qkc5wzi9683l5xzzq1i705l3adiwkdfkcbjjf";
  libraryHaskellDepends = [
    base base-compat bytestring containers deepseq exceptions
    http-client http-media http-types kan-extensions monad-control mtl
    semigroupoids servant servant-client-core stm text time
    transformers transformers-base transformers-compat
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring entropy hspec http-api-data
    http-client http-types HUnit kan-extensions markdown-unlit mtl
    network QuickCheck servant servant-client-core servant-server stm
    tdigest text transformers transformers-compat wai warp
  ];
  testToolDepends = [ hspec-discover markdown-unlit ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://docs.servant.dev/";
  description = "Automatic derivation of querying functions for servant";
  license = stdenv.lib.licenses.bsd3;
}
