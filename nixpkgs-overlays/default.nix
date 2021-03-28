self: super:
let pkgs = super;
in rec {
  android-activity = self.callPackage ../derivations/android-activity.nix {};
  secp256k1Sys = self.callPackage ../derivations/secp256k1Sys.nix {};
  zlibSys = self.callPackage ../derivations/zlibSys.nix {};
  lmdbSys = self.callPackage ../derivations/lmdbSys.nix {};
  leveldb = self.callPackage ../derivations/leveldb.nix {};
  p11-kit = self.callPackage ../derivations/p11-kit.nix {};
}
