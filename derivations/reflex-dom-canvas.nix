{ mkDerivation, base, bifunctors, containers, fetchgit, free
, ghcjs-dom, jsaddle, lens, mtl, random, reflex, reflex-dom-core
, stdenv, text, time
}:
mkDerivation {
  pname = "reflex-dom-canvas";
  version = "0.4.0.1";
  src = ../reflex-dom-canvas;
  postUnpack = "sourceRoot+=/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bifunctors containers free ghcjs-dom jsaddle lens mtl random
    reflex reflex-dom-core text time
  ];
  description = "Reflex functions for the HTML5 Canvas (2D & WebGL)";
  license = stdenv.lib.licenses.bsd3;
}
