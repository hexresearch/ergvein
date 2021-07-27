{ mkDerivation, ansi-terminal, base, bytestring, containers, Diff
, dlist, exceptions, filepath, ghc-lib-parser, gitrev, hspec
, hspec-discover, mtl, optparse-applicative, path, path-io, syb
, text, lib
}:
mkDerivation {
  pname = "ormolu";
  version = "0.1.4.1";
  sha256 = "1aamgzimjn9h7kwby9ajfgbj5dx08nmxyalwvpg9rs4xd8pbpd9s";
  revision = "1";
  editedCabalFile = "1fi8fxyhw9jdwhsbmrikjqd461wrz7h4kdszrahlvdjfdsn4wh7d";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring containers Diff dlist exceptions
    ghc-lib-parser mtl syb text
  ];
  executableHaskellDepends = [
    base filepath ghc-lib-parser gitrev optparse-applicative text
  ];
  testHaskellDepends = [
    base containers filepath hspec path path-io text
  ];
  testToolDepends = [ hspec-discover ];
  description = "A formatter for Haskell source code";
  license = lib.licenses.bsd3;
}