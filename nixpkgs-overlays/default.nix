self: super:
{
  android-activity = self.callPackage ../derivations/android-activity.nix {};
  /* jdk = self.callPackage ./derivations/jdk.nix {}; */
  secp256k1Sys = self.callPackage ../derivations/secp256k1Sys.nix {};
  zlibSys = self.callPackage ../derivations/zlibSys.nix {};
  lmdbSys = self.callPackage ../derivations/lmdbSys.nix {};
}
