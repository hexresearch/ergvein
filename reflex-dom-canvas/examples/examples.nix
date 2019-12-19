{ mkDerivation, base, containers, jsaddle, ghcjs-dom, lens, mtl
, random, reflex, reflex-dom, reflex-dom-canvas, stdenv, text, time
}:
mkDerivation {
  pname = "examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers jsaddle ghcjs-dom lens mtl random reflex
    reflex-dom reflex-dom-canvas text time
  ];
  license = stdenv.lib.licenses.bsd3;
}
