{ mkDerivation, array, base, fetchgit, lmdbSys, stdenv }:
mkDerivation {
  pname = "lmdb";
  version = "0.2.5";
  src = fetchgit {
    url = "https://github.com/hexresearch/haskell-lmdb.git";
    sha256 = "07k3mg3jy8ajygj1zq7j0521b92nk9mmcds52mx48vg6lnny8rih";
    rev = "895c65bfffcbb55c34a35c28abfc84bc7d519477";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ array base ];
  librarySystemDepends = [ lmdbSys ];
  homepage = "http://github.com/dmbarbour/haskell-lmdb";
  description = "Lightning MDB bindings";
  license = stdenv.lib.licenses.bsd2;
}
