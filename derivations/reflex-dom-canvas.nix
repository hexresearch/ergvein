{ mkDerivation, base, bifunctors, containers, free, ghcjs-dom
, jsaddle, lens, mtl, random, reflex, reflex-dom-core, stdenv, text
, time, fetchgit
}:
mkDerivation {
  pname = "reflex-dom-canvas";
  version = "0.4.0.1";
  src = fetchgit {
    url = "https://github.com/hexresearch/reflex-dom-canvas.git";
    sha256 = "1slq8gmhy8agjssyzfs6y0sna427b7lwvvbqps9w87h8h0d3z1pr";
    rev = "2d067bb9e806aba5a749d74b175aaf154cf0945d";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bifunctors containers free ghcjs-dom jsaddle lens mtl random
    reflex reflex-dom-core text time
  ];
  description = "Reflex functions for the HTML5 Canvas (2D & WebGL)";
  license = stdenv.lib.licenses.bsd3;
}
