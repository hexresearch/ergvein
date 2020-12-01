self: super:
rec {
  android-activity = self.callPackage ../derivations/android-activity.nix {};
  secp256k1Sys = self.callPackage ../derivations/secp256k1Sys.nix {};
  zlibSys = self.callPackage ../derivations/zlibSys.nix {};
  lmdbSys = self.callPackage ../derivations/lmdbSys.nix {};
  python = super.python.override {
    packageOverrides = python-self: python-super: {
      pyopenssl = python-super.pyopenssl.overrideAttrs (oldAttrs: {
        doCheck = false;
      });
    };
  };
}
