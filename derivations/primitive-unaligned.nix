{ mkDerivation, base, primitive, stdenv }:
mkDerivation {
  pname = "primitive-unaligned";
  version = "0.1.1.1";
  sha256 = "14322b85b3cd12221cc15de323dee8f7cefe65bab647b3d00cfde4969a216ab8";
  libraryHaskellDepends = [ base primitive ];
  testHaskellDepends = [ base primitive ];
  homepage = "https://github.com/haskell-primitive/primitive-unaligned";
  description = "Unaligned access to primitive arrays";
  license = stdenv.lib.licenses.bsd3;
}
