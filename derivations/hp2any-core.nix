{ mkDerivation, attoparsec, base, bytestring, containers, directory
, fetchgit, filepath, network, process, stdenv, time
}:
mkDerivation {
  pname = "hp2any-core";
  version = "0.11.2";
  src = fetchgit {
    url = "https://github.com/hexresearch/hp2any";
    sha256 = "0528lid5d5ihbxspq8wrf292zjy8f25amwkhzzizcyzsm3yaw8mw";
    rev = "a4c1a4b46b44ee204e63fc8beca684af539d533e";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    attoparsec base bytestring containers directory filepath network
    process time
  ];
  homepage = "http://www.haskell.org/haskellwiki/Hp2any";
  description = "Heap profiling helper library";
  license = stdenv.lib.licenses.bsd3;
}
