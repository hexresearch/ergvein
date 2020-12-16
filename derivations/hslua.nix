{ mkDerivation, base, bytestring, containers, criterion, deepseq
, exceptions, fail, lua5_3, mtl, QuickCheck, quickcheck-instances
, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "hslua";
  version = "1.0.3.2";
  sha256 = "d2d40b0c143ef58d26203f34d96d4220f1a20077386e4a5216be74260b7d6ba0";
  configureFlags = [ "-fsystem-lua" "-f-use-pkgconfig" ];
  libraryHaskellDepends = [
    base bytestring containers exceptions fail mtl text
  ];
  librarySystemDepends = [ lua5_3 ];
  testHaskellDepends = [
    base bytestring containers exceptions fail mtl QuickCheck
    quickcheck-instances tasty tasty-hunit tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [ base bytestring criterion deepseq ];
  homepage = "https://hslua.github.io/";
  description = "Bindings to Lua, an embeddable scripting language";
  license = stdenv.lib.licenses.mit;
}
