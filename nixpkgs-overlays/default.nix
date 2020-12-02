self: super:
let pkgs = super;
    overrideOpenssl = self: super: {
      pyopenssl = super.pyopenssl.overridePythonAttrs(old: rec {
        doCheck = false;
        nativeBuildInputs = [ pkgs.openssl.dev ];
      });
    };
in rec {
  android-activity = self.callPackage ../derivations/android-activity.nix {};
  secp256k1Sys = self.callPackage ../derivations/secp256k1Sys.nix {};
  zlibSys = self.callPackage ../derivations/zlibSys.nix {};
  lmdbSys = self.callPackage ../derivations/lmdbSys.nix {};

  python2 = super.python2.override {
    packageOverrides = overrideOpenssl;
  };
  python2Packages = super.recurseIntoAttrs (python2.pkgs);

  python3 = super.python3.override {
    packageOverrides = overrideOpenssl;
  };
  python3Packages = super.recurseIntoAttrs (python3.pkgs);

  python37 = super.python37.override {
    packageOverrides = overrideOpenssl;
  };
  python37Packages = super.recurseIntoAttrs (python37.pkgs);

  python = super.python.override {
    packageOverrides = overrideOpenssl;
  };
  pythonPackages = super.recurseIntoAttrs (python.pkgs);
  pyopenssl = pythonPackages.pyopenssl;
}
