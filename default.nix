{ release ? false, profile ? false }:
let
   reflex-platform = import ./platform-overlay.nix { inherit profile; };
   project = reflex-platform.project ({ pkgs, ... }: {
    packages = {
      cbitstream = ./cbitstream;
      data-merkle-tree = ./data-merkle-tree;
      ergo-api = ./ergo-api;
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
      ergvein-website = ./ergvein-website;
      golomb-rice = ./golomb-rice;
      reflex-dom-canvas = ./reflex-dom-canvas;
      reflex-dom-retractable = ./retractable;
      reflex-external-ref = ./reflex-external-ref;
      reflex-localize = ./reflex-localize;
      x509-android = ./x509-android;
    };
    shells = {
      ghc = [
        "cbitstream"
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
        "ergvein-website"
        "golomb-rice"
        "reflex-dom-retractable"
        "reflex-external-ref"
        "reflex-localize"
      ];
    };
    overrides = import ./overrides.nix { inherit reflex-platform; };

    shellToolOverrides = ghc: super: {
      inherit (pkgs) postgresql leveldb;
      inherit (pkgs.haskellPackages) hakyll;
      hp2any-graph = if profile then ghc.hp2any-graph else null;
    };

    android.ergvein-wallet = {
      executableName = "ergvein";
      applicationId = "org.ergvein.wallet";
      displayName = "Ergvein wallet";
      resources = ./wallet/static/res;
      assets = ./wallet/static/assets;
      iconPath = "@drawable/ic_launcher";
      runtimeSharedLibs = nixpkgs: [
        "${nixpkgs.zlibSys}/lib/libz.so"
        "${nixpkgs.secp256k1Sys}/lib/libsecp256k1.so"
        "${nixpkgs.lmdbSys}/lib/liblmdb.so"
      ];
      /* additionalDependencies = ''

      ''; */
      javaSources = [
        ./wallet/java
        "${project.ghc.x509-android.src}/java"
      ];
      version = import ./android-version.nix;
      releaseKey = let
        readPassword = file: builtins.replaceStrings ["\n"] [""] (builtins.readFile file);
      in if release then {
        storeFile = ./release/release.keystore;
        storePassword = readPassword ./release/password;
        keyAlias = "ergvein_releasekey";
        keyPassword = readPassword ./release/password;
      } else null;
    };

  });
  in project
