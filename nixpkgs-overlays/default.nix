self: super:
{
  android-activity = self.callPackage ./derivations/android-activity.nix {};
  secp256k1 = self.callPackage ../derivations/secp256k1.nix {};
  zlibSys = self.callPackage ../derivations/zlibSys.nix {};
}
