{ mkDerivation, array, base, fetchgit, lmdbSys, stdenv }:
mkDerivation {
  pname = "lmdb";
  version = "0.2.5";
  src = fetchgit {
    url = "https://github.com/hexresearch/haskell-lmdb.git";
    sha256 = "1rspfklhw4mzph4pi9hpqs6k9d1bjdskjziii6bdgznnybkyw6yc";
    rev = "1e562429874919d445576c87cf118d7de5112b5b";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ array base ];
  librarySystemDepends = [ lmdbSys ];
  homepage = "http://github.com/dmbarbour/haskell-lmdb";
  description = "Lightning MDB bindings";
  license = stdenv.lib.licenses.bsd2;
}
