self: super:
rec {
  android-activity = self.callPackage ../derivations/android-activity.nix {};
  secp256k1Sys = self.callPackage ../derivations/secp256k1Sys.nix {};
  zlibSys = self.callPackage ../derivations/zlibSys.nix {};
  lmdbSys = self.callPackage ../derivations/lmdbSys.nix {};

  python2 = super.python2.override {
    # Careful, we're using a different self and super here!
    packageOverrides = self: super: {
      pyopenssl = super.pyopenssl.overridePythonAttrs(old: rec {
        checkPhase = "";
      });
    };
  };
  python2Packages = super.recurseIntoAttrs (python2.pkgs);

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
