{ release ? false, isAndroid ? false }:
let
  reflex-platform = (import ./reflex-platform.nix) {
    nixpkgsOverlays = [
      (self: super: import ./nixpkgs-overlays/default.nix self super)
    ];
    config.android_sdk.accept_license = true;
  };
in reflex-platform.project ({ pkgs, ... }: {
  packages = {
    ergvein-common = ./common;
    ergvein-crypto = ./crypto;
    ergvein-index-api = ./index-api;
    ergvein-index-server = ./index-server;
    ergvein-wallet = ./wallet;
    ergvein-wallet-android = ./wallet-android;
    ergvein-wallet-desktop = ./wallet-desktop;
    ergvein-wallet-native = ./wallet-native;
    reflex-dom-retractable = ./retractable;
    reflex-external-ref = ./reflex-external-ref;
    reflex-localize = ./reflex-localize;
  };
  shells = {
    ghc = [
      "ergvein-common"
      "ergvein-crypto"
      "ergvein-index-api"
      "ergvein-index-server"
      "ergvein-wallet"
      "ergvein-wallet-native"
      "ergvein-wallet-desktop"
      "reflex-dom-retractable"
      "reflex-external-ref"
      "reflex-localize"
    ];
  };
  overrides = import ./overrides.nix { inherit reflex-platform isAndroid; };

  shellToolOverrides = ghc: super: {
    inherit (pkgs) postgresql;
  };

  android.ergvein-wallet = {
    executableName = "ergvein";
    applicationId = "org.ergvein.wallet";
    displayName = "Ergvein wallet";
    resources = ./wallet/static/res;
    assets = ./wallet/static/assets;
    iconPath = "@drawable/ic_launcher";
    nativeDependencies = nixpkgs: haskellPackages: {
      "libsecp256k1.so" = "${nixpkgs.secp256k1}/lib/libsecp256k1.so";
    };
    javaSources = _: [
      ./wallet/java
    ];
    version = {
      code = "1";
      name = "Alpha";
    };
    releaseKey = let
      readPassword = file: builtins.replaceStrings ["\n"] [""] (builtins.readFile file);
    in if release then {
      storeFile = ./release/release.keystore;
      storePassword = readPassword ./release/password;
      keyAlias = "ergvein_releasekey";
      keyPassword = readPassword ./release/password;
    } else null;
  };

})
