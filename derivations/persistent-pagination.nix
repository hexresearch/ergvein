{ mkDerivation, base, conduit, containers, esqueleto, foldl, hspec
, hspec-discover, microlens, mtl, persistent, persistent-sqlite
, persistent-template, QuickCheck, stdenv, time
}:
mkDerivation {
  pname = "persistent-pagination";
  version = "0.1.1.0";
  sha256 = "0a9e10b196ec795d16c3b7e56fdb4ddefb16040928f69b33e9cfb7b49fb055bc";
  libraryHaskellDepends = [
    base conduit esqueleto foldl microlens mtl persistent
  ];
  testHaskellDepends = [
    base conduit containers esqueleto hspec hspec-discover mtl
    persistent persistent-sqlite persistent-template QuickCheck time
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/parsonsmatt/persistent-pagination#readme";
  description = "Efficient and correct pagination for persistent or esqueleto queries";
  license = stdenv.lib.licenses.bsd3;
}
