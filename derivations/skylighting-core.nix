{ mkDerivation, aeson, ansi-terminal, attoparsec, base
, base64-bytestring, binary, blaze-html, bytestring
, case-insensitive, colour, containers, criterion, Diff, directory
, filepath, HUnit, hxt, mtl, pretty-show, QuickCheck, random
, regex-pcre-builtin, safe, stdenv, tasty, tasty-golden
, tasty-hunit, tasty-quickcheck, text, transformers, utf8-string
}:
mkDerivation {
  pname = "skylighting-core";
  version = "0.8.1";
  sha256 = "abb161ca7a4fc07fb44f1c8b8adc849051512dac50ad2325d6f723419e233ca1";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-terminal attoparsec base base64-bytestring binary
    blaze-html bytestring case-insensitive colour containers directory
    filepath hxt mtl regex-pcre-builtin safe text transformers
    utf8-string
  ];
  testHaskellDepends = [
    aeson base bytestring containers Diff directory filepath HUnit
    pretty-show QuickCheck random tasty tasty-golden tasty-hunit
    tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [
    base containers criterion directory filepath text
  ];
  doCheck = false;
  homepage = "https://github.com/jgm/skylighting";
  description = "syntax highlighting library";
  license = stdenv.lib.licenses.bsd3;
}
