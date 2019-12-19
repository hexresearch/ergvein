{ mkDerivation, base, bifunctors, containers, free, ghcjs-dom
, jsaddle, lens, mtl, random, reflex, reflex-dom-core, stdenv, text
, time, fetchgit
}:
mkDerivation {
  pname = "reflex-dom-canvas";
  version = "0.4.0.1";
  src = fetchgit {
    url = "https://github.com/hexresearch/reflex-dom-canvas.git";
    sha256 = "16lv03djblzlpw7i4clzpw7417pqbplxw9qwm9ir2ap5xwnsrhdf";
    rev = "6c9d7ee534c8b17db3520ee2f56a0d3693080a28";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bifunctors containers free ghcjs-dom jsaddle lens mtl random
    reflex reflex-dom-core text time
  ];
  description = "Reflex functions for the HTML5 Canvas (2D & WebGL)";
  license = stdenv.lib.licenses.bsd3;
}
