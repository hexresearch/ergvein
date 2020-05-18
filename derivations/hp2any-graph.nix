{ mkDerivation, base, bytestring, containers, directory, fetchgit
, filepath, freeglut, GLUT, hp2any-core, libGL, libGLU, network
, OpenGL, parseargs, process, stdenv
}:
mkDerivation {
  pname = "hp2any-graph";
  version = "0.5.4.2";
  src = fetchgit {
    url = "https://github.com/hexresearch/hp2any";
    sha256 = "0528lid5d5ihbxspq8wrf292zjy8f25amwkhzzizcyzsm3yaw8mw";
    rev = "a4c1a4b46b44ee204e63fc8beca684af539d533e";
    fetchSubmodules = true;
  };
  /* src = ../../hp2any; */
  postUnpack = "sourceRoot+=/graph; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base hp2any-core OpenGL ];
  executableHaskellDepends = [
    base bytestring containers directory filepath GLUT hp2any-core
    network OpenGL parseargs process
  ];
  executableSystemDepends = [ freeglut libGL libGLU ];
  homepage = "http://www.haskell.org/haskellwiki/Hp2any";
  description = "Real-time heap graphing utility and profile stream server with a reusable graphing module";
  license = stdenv.lib.licenses.bsd3;
}
