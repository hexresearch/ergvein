{ release ? false
, profile ? false
, gitHash ? null
, releaseBundle ? true
 }:
let
   reflex-platform = import ./platform-overlay.nix { inherit profile; };
   version = import ./android-version.nix;
   versionTag = version.code;
   project = reflex-platform.project ({ pkgs, ... }: {
    packages = {
      cbitstream = ./cbitstream;
      coinbase-client = ./coinbase-client;
      data-merkle-tree = ./data-merkle-tree;
      ergo-api = ./ergo-api;
      ergvein-checkpoint-generator = ./checkpoint-generator;
      ergvein-common = ./common;
      ergvein-crypto = ./crypto;
      ergvein-index-api = ./index-api;
      ergvein-index-client = ./index-client;
      ergvein-index-protocol = ./index-protocol;
      ergvein-index-protocol-client = ./index-protocol-client;
      ergvein-index-server = ./index-server;
      ergvein-interface-ergo = ./interfaces/ergo;
      ergvein-node-discovery = ./node-discovery;
      ergvein-wallet = ./wallet;
      ergvein-wallet-filters = ./wallet-filters;
      ergvein-wallet-types = ./wallet-types;
      ergvein-wallet-version = ./wallet-version;
      ergvein-website = ./ergvein-website;
      golomb-rice = ./golomb-rice;
      reflex-dom-canvas = ./reflex-dom-canvas;
      reflex-dom-retractable = ./retractable;
      reflex-external-ref = ./reflex-external-ref;
      reflex-fork = ./reflex-fork;
      reflex-localize = ./reflex-localize;
      reflex-localize-dom = ./reflex-localize-dom;
      sepulcas = ./sepulcas;
      sepulcas-android = ./sepulcas-android;
      sepulcas-desktop = ./sepulcas-desktop;
      sepulcas-log = ./sepulcas-log;
      sepulcas-native = ./sepulcas-native;
      ui-playground = ./ui-playground;
      x509-android = ./x509-android;
    };
    shells = {
      ghc = [
        "cbitstream"
        "coinbase-client"
        "data-merkle-tree"
        "ergo-api"
        "ergvein-checkpoint-generator"
        "ergvein-common"
        "ergvein-crypto"
        "ergvein-index-api"
        "ergvein-index-client"
        "ergvein-index-protocol-client"
        "ergvein-index-protocol"
        "ergvein-index-server"
        "ergvein-interface-ergo"
        "ergvein-node-discovery"
        "ergvein-wallet-filters"
        "ergvein-wallet-types"
        "ergvein-wallet-version"
        "ergvein-wallet"
        "ergvein-website"
        "golomb-rice"
        "reflex-dom-retractable"
        "reflex-external-ref"
        "reflex-fork"
        "reflex-localize"
        "reflex-localize-dom"
        "sepulcas-desktop"
        "sepulcas-log"
        "sepulcas-native"
        "sepulcas"
        "ui-playground"
      ];
    };
    overrides = import ./overrides.nix { inherit reflex-platform gitHash versionTag; };

    shellToolOverrides = ghc: super: {
      inherit (pkgs) leveldb;
      inherit (pkgs.haskellPackages) hakyll;
      hp2any-graph = if profile then ghc.hp2any-graph else null;
      ghc-prof-flamegraph = if profile then ghc.ghc-prof-flamegraph else null;
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
        "${project.ghc.sepulcas-android.src}/java"
        "${project.ghc.x509-android.src}/java"
      ];
      version = version;
      inherit releaseBundle;
      releaseKey = let
        readPassword = file: builtins.replaceStrings ["\n"] [""] (builtins.readFile file);
      in if release then {
        storeFile = ./release/release.keystore;
        storePassword = readPassword ./release/password;
        keyAlias = "ergvein_releasekey";
        keyPassword = readPassword ./release/password;
      } else null;
      services = ''
      <provider
          android:name="androidx.core.content.FileProvider"
          android:authorities="org.ergvein.fileprovider"
          android:exported="false"
          android:grantUriPermissions="true">
          <meta-data
              android:name="android.support.FILE_PROVIDER_PATHS"
              android:resource="@xml/file_paths" />
      </provider>
      '';
      permissions = ''
      <uses-permission android:name="android.permission.ACCESS_NETWORK_STATE" />
      '';
    };

  });
  in project
