{ mkDerivation, base, bifunctors, containers, free, ghcjs-dom
, jsaddle, lens, mtl, random, reflex, reflex-dom-core, stdenv, text
, time, fetchgit
}:
mkDerivation {
  pname = "reflex-dom-canvas";
  version = "0.4.0.1";
  src = fetchgit {
    url = "https://github.com/qfpl/reflex-dom-canvas.git";
    #sha256 = "01jz1aqrrrg6x5q108axa3n7yhfrd1c72gj67xkjbpj35hwja1y9";
    rev = "b9131dffd521e4534eb2240e3fe0b7bfb70136a8";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bifunctors containers free ghcjs-dom jsaddle lens mtl random
    reflex reflex-dom-core text time
  ];
  description = "Reflex functions for the HTML5 Canvas (2D & WebGL)";
  license = stdenv.lib.licenses.bsd3;
}
