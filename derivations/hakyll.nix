{ mkDerivation, base, binary, blaze-html, blaze-markup, bytestring
, containers, cryptonite, data-default, deepseq, directory
, file-embed, filepath, fsnotify, http-conduit, http-types, lib
, lrucache, memory, mtl, network-uri, optparse-applicative, pandoc
, pandoc-citeproc, parsec, process, QuickCheck, random, regex-tdfa
, resourcet, scientific, tagsoup, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, text, time
, time-locale-compat, unordered-containers, vector, wai
, wai-app-static, warp, yaml
}:
mkDerivation {
  pname = "hakyll";
  version = "4.13.4.0";
  sha256 = "a30a4810546ef95d011535dfbc82038e50fff25680ac5ab56bd6ab5b653a9a3d";
  revision = "1";
  editedCabalFile = "1crs1rh886srl97xy4mgi0xgwh8wqjlal9ily2gyppwc71q4fprv";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base binary blaze-html blaze-markup bytestring containers
    cryptonite data-default deepseq directory file-embed filepath
    fsnotify http-conduit http-types lrucache memory mtl network-uri
    optparse-applicative pandoc pandoc-citeproc parsec process random
    regex-tdfa resourcet scientific tagsoup template-haskell text time
    time-locale-compat unordered-containers vector wai wai-app-static
    warp yaml
  ];
  executableHaskellDepends = [ base directory filepath ];
  testHaskellDepends = [
    base bytestring containers filepath QuickCheck tasty tasty-hunit
    tasty-quickcheck text unordered-containers yaml
  ];
  homepage = "http://jaspervdj.be/hakyll";
  description = "A static website compiler library";
  license = lib.licenses.bsd3;
}
