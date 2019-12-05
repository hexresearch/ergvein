self: super:
{
  secp256k1 = self.callPackage ../derivations/secp256k1.nix {};
  zlibSys = self.callPackage ../derivations/zlibSys.nix {};
}
