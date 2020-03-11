{ mkDerivation, array, base, lmdbSys, pkg-config, stdenv }:
mkDerivation {
  pname = "lmdb";
  version = "0.2.5";
  src = ../../haskell-lmdb;
  libraryHaskellDepends = [ array base ];
  librarySystemDepends = [ lmdbSys ];
  homepage = "http://github.com/dmbarbour/haskell-lmdb";
  description = "Lightning MDB bindings";
  license = stdenv.lib.licenses.bsd2;
}
