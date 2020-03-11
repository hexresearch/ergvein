{ mkDerivation, base, data-default, jdk, stdenv }:
mkDerivation {
  pname = "android-activity";
  version = "0.1";
  src = ../android-activity;
  /* postUnpack = "sourceRoot+=/android-activity; echo source root reset to $sourceRoot"; */
  libraryHaskellDepends = [ base data-default ];
  # librarySystemDepends = [ jdk ];
  homepage = "https://github.com/obsidiansystems/android-activity";
  description = "Turn regular Haskell programs into Android Activities";
  license = stdenv.lib.licenses.bsd3;
}
