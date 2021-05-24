{ mkDerivation, array, base, bytestring, directory, fetchgit
, filepath, ghc-prim, hspec, json, language-rust, prettyprinter
, process, random, stdenv, template-haskell, transformers
, rustc-nightly, cargo-nightly
}:
mkDerivation {
  pname = "inline-rust";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/hexresearch/inline-rust";
    sha256 = "16y2visyyy0vfr9v9pl71822g7v3hav3b83nfk0imp2x7dvabgwm";
    rev = "4eae45cf1786a23fdb9460dbd2dbb4847e273d3e";
    fetchSubmodules = true;
  };
  /* src = ../../inline-rust; */
  buildTools = [ rustc-nightly cargo-nightly ];
  doCheck = false;
  doHaddock = false;
  libraryHaskellDepends = [
    array base bytestring directory filepath json language-rust
    prettyprinter process random template-haskell transformers
  ];
  testHaskellDepends = [ base ghc-prim hspec language-rust ];
  homepage = "https://github.com/harpocrates/inline-rust";
  description = "Haskell library for inline rust quasiquotes";
  license = stdenv.lib.licenses.bsd3;
}
