{ mkDerivation, base, bifunctors, containers, free, ghcjs-dom
, jsaddle, lens, mtl, random, reflex, reflex-dom-core, stdenv, text
, time
}:
mkDerivation {
  pname = "reflex-dom-canvas";
  version = "0.4.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors containers free ghcjs-dom jsaddle lens mtl random
    reflex reflex-dom-core text time
  ];
  description = "Reflex functions for the HTML5 Canvas (2D & WebGL)";
  license = stdenv.lib.licenses.bsd3;
}
