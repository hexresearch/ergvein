{ mkDerivation, array, base, bytestring, directory, fetchgit
, filepath, ghc-prim, hspec, json, language-rust, prettyprinter
, process, random, stdenv, template-haskell, transformers
}:
mkDerivation {
  pname = "inline-rust";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/harpocrates/inline-rust";
    sha256 = "1ni3bhlj2wv6i17lzihrfj2fqjh6k8ppicrqwmvk6r3r1cjc8gav";
    rev = "5ecff8c92526000e5fc358a2dfede9b60ef59a1a";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    array base bytestring directory filepath json language-rust
    prettyprinter process random template-haskell transformers
  ];
  testHaskellDepends = [ base ghc-prim hspec language-rust ];
  homepage = "https://github.com/harpocrates/inline-rust";
  description = "Haskell library for inline rust quasiquotes";
  license = stdenv.lib.licenses.bsd3;
}
