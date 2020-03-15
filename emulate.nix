let
  reflex-platform = (import ./reflex-platform.nix) {
    nixpkgsOverlays = [
      (self: super: import ./nixpkgs-overlays/default.nix self super)
    ];
    config.android_sdk.accept_license = true;
    config.oraclejdk.accept_license = true;
    config.allowUnfree = true;
  };
  project = (import ./default.nix) { isAndroid = true; };
  pkgs = reflex-platform.nixpkgs;
  ergvein-wallet = project.android.ergvein-wallet;
  ergvein-wallet-app = pkgs.stdenv.mkDerivation {
    name = "ergvein-wallet-app";
    buildCommand = ''
      mkdir -p $out
      cp ${ergvein-wallet}/debug/*.apk $out
    '';
  };
  emulateApp = import ./nix/emulate-app.nix {
    inherit (pkgs) stdenv;
    inherit (pkgs.androidenv) composeAndroidPackages;
  };
in emulateApp {
    name = "emulate-ergvein";
    /* platformVersion = "28";
    abiVersion = "x86_64"; */
    platformVersion = "24";
    abiVersion = "armeabi-v7a";
    app = ergvein-wallet-app;
    package = "org.ergvein.wallet";
    activity = "HaskellActivity";
    enableGPU = true;
}
