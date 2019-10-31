{ mkDerivation, base, binary, bytestring, stdenv }:
mkDerivation {
  pname = "bytestring-trie";
  version = "0.2.5.0";
  sha256 = "0891bd31ff60670ab6849d6dd04a73095e20e7e6bab94818857dc6acfcb5feca";
  libraryHaskellDepends = [ base binary bytestring ];
  homepage = "http://wrengr.org";
  description = "An efficient finite map from (byte)strings to values";
  license = stdenv.lib.licenses.bsd3;
}
