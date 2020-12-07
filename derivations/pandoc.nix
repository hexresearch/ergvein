{ mkDerivation, aeson, aeson-pretty, attoparsec, base
, base64-bytestring, binary, blaze-html, blaze-markup, bytestring
, case-insensitive, cmark-gfm, containers, criterion, data-default
, deepseq, Diff, directory, doctemplates, exceptions
, executable-path, filepath, Glob, haddock-library, hslua
, hslua-module-system, hslua-module-text, HsYAML, HTTP, http-client
, http-client-tls, http-types, ipynb, JuicyPixels, mtl, network
, network-uri, pandoc-types, parsec, process, QuickCheck, random
, safe, scientific, SHA, skylighting, split, stdenv, syb, tagsoup
, tasty, tasty-golden, tasty-hunit, tasty-lua, tasty-quickcheck
, temporary, texmath, text, time, unicode-transforms, unix
, unordered-containers, vector, weigh, xml, zip-archive, zlib
}:
mkDerivation {
  pname = "pandoc";
  version = "2.7.3";
  sha256 = "a877203379ec5179716d6999f76352229d7f40f5bec70dbfa48c140848cef236";
  revision = "1";
  editedCabalFile = "124is72dmd98fk8mayzird9i9px619kzszm2qrl1dacy0wcn6ri8";
  configureFlags = [ "-fhttps" "-f-trypandoc" ];
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty attoparsec base base64-bytestring binary
    blaze-html blaze-markup bytestring case-insensitive cmark-gfm
    containers data-default deepseq directory doctemplates exceptions
    filepath Glob haddock-library hslua hslua-module-system
    hslua-module-text HsYAML HTTP http-client http-client-tls
    http-types ipynb JuicyPixels mtl network network-uri pandoc-types
    parsec process random safe scientific SHA skylighting split syb
    tagsoup temporary texmath text time unicode-transforms unix
    unordered-containers vector xml zip-archive zlib
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base base64-bytestring bytestring containers Diff directory
    executable-path filepath Glob hslua pandoc-types process QuickCheck
    tasty tasty-golden tasty-hunit tasty-lua tasty-quickcheck temporary
    text time xml zip-archive
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion mtl text time weigh
  ];
  postInstall = ''
    mkdir -p $out/share/man/man1
    mv "man/"*.1 $out/share/man/man1/
  '';
  homepage = "https://pandoc.org";
  description = "Conversion between markup formats";
  license = stdenv.lib.licenses.gpl2;
}
