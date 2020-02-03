{ release ? false, isAndroid ? false }:
let
  reflex-platform = (import ./reflex-platform.nix) {
    nixpkgsOverlays = [
      (self: super: import ./nixpkgs-overlays/default.nix self super )
    ];
    config = {
      android_sdk.accept_license = true;
      allowBroken = true;
    };
  };
in reflex-platform.project ({ pkgs, ... }: {
  packages = {
    data-merkle-tree = ./data-merkle-tree;
    ergvein-checkpoint-generator = ./checkpoint-generator;
    ergvein-common = ./common;
    ergvein-crypto = ./crypto;
    ergvein-index-api = ./index-api;
    ergvein-index-client = ./index-client;
    ergvein-index-server = ./index-server;
    ergvein-interface-ergo = ./interfaces/ergo;
    ergvein-wallet = ./wallet;
    ergvein-wallet-android = ./wallet-android;
    ergvein-wallet-desktop = ./wallet-desktop;
    ergvein-wallet-filters = ./wallet-filters;
    ergvein-wallet-native = ./wallet-native;
    ergvein-wallet-types = ./wallet-types;
    ergo-api = ./ergo-api;
    golomb-rice = ./golomb-rice;
    reflex-dom-retractable = ./retractable;
    reflex-external-ref = ./reflex-external-ref;
    reflex-localize = ./reflex-localize;
  };
  shells = {
    ghc = [
      "data-merkle-tree"
      "ergo-api"
      "ergvein-checkpoint-generator"
      "ergvein-common"
      "ergvein-crypto"
      "ergvein-index-api"
      "ergvein-index-client"
      "ergvein-index-server"
      "ergvein-interface-ergo"
      "ergvein-wallet-desktop"
      "ergvein-wallet-filters"
      "ergvein-wallet-native"
      "ergvein-wallet"
      "golomb-rice"
      "reflex-dom-retractable"
      "reflex-external-ref"
      "reflex-localize"
    ];
  };
  overrides = import ./overrides.nix { inherit reflex-platform isAndroid; };

  shellToolOverrides = ghc: super: {
    inherit (pkgs) postgresql leveldb;
  };

  android.ergvein-wallet = {
    executableName = "ergvein";
    applicationId = "org.ergvein.wallet";
    displayName = "Ergvein wallet";
    resources = ./wallet/static/res;
    assets = ./wallet/static/assets;
    iconPath = "@drawable/ic_launcher";
    nativeDependencies = nixpkgs: haskellPackages: {
      "libz.so" = "${nixpkgs.zlib}/lib/libz.so.1";
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
