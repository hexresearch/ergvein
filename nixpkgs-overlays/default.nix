self: super:
rec {
  android-activity = self.callPackage ../derivations/android-activity.nix {};
  secp256k1Sys = self.callPackage ../derivations/secp256k1Sys.nix {};
  zlibSys = self.callPackage ../derivations/zlibSys.nix {};
  lmdbSys = self.callPackage ../derivations/lmdbSys.nix {};

  python = super.python.override {
    # Careful, we're using a different self and super here!
    packageOverrides = self: super: {
      pyopenssl = super.pyopenssl.overridePythonAttrs(old: rec {
        checkPhase = "";
      });
    };
  };
  pythonPackages = super.recurseIntoAttrs (python.pkgs);
  pyopenssl = pythonPackages.pyopenssl;
}
