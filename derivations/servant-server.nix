{ mkDerivation, aeson, base, base-compat, base64-bytestring
, bytestring, Cabal, cabal-doctest, containers, directory, doctest
, exceptions, filepath, hspec, hspec-discover, hspec-wai
, http-api-data, http-media, http-types, monad-control, mtl
, network, network-uri, QuickCheck, resourcet, safe, servant
, should-not-typecheck, stdenv, string-conversions, tagged
, temporary, text, transformers, transformers-base
, transformers-compat, wai, wai-app-static, wai-extra, warp, word8
}:
mkDerivation {
  pname = "servant-server";
  version = "0.16";
  sha256 = "6b6cbd3c9bda25d1b44501b6fb874b7c66cccc057a4b1636ab8890e5614a3ba3";
  revision = "2";
  editedCabalFile = "0b5vm4pq55ii8mgv1pig53wmixwph73kknkdqvqgad21mq01gqp9";
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base base-compat base64-bytestring bytestring containers exceptions
    filepath http-api-data http-media http-types monad-control mtl
    network network-uri resourcet servant string-conversions tagged
    text transformers transformers-base wai wai-app-static word8
  ];
  executableHaskellDepends = [
    aeson base base-compat servant text wai warp
  ];
  testHaskellDepends = [
    aeson base base-compat base64-bytestring bytestring directory
    doctest hspec hspec-wai http-types mtl QuickCheck resourcet safe
    servant should-not-typecheck string-conversions temporary text
    transformers transformers-compat wai wai-extra
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://docs.servant.dev/";
  description = "A family of combinators for defining webservices APIs and serving them";
  license = stdenv.lib.licenses.bsd3;
}
