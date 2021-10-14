{ release ? false
, profile ? false
, gitHash ? null
, releaseBundle ? true
, releasePasswordFile ? ./release/password
, releaseKeyStore ? ./release/release.keystore
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
      ergvein-checkpoint-generator = ./checkpoint-generator;
      ergvein-common = ./common;
      ergvein-core = ./ergvein-core;
      ergvein-crypto = ./crypto;
      ergvein-filters = ./wallet-filters;
      ergvein-index-api = ./index-api;
      ergvein-index-client = ./index-client;
      ergvein-index-protocol = ./index-protocol;
      ergvein-index-protocol-client = ./index-protocol-client;
      ergvein-index-server = ./index-server;
      ergvein-localize = ./ergvein-localize;
      ergvein-node-discovery = ./node-discovery;
      ergvein-types = ./wallet-types;
      ergvein-wallet = ./wallet;
      ergvein-wallet-version = ./wallet-version;
      ergvein-website = ./ergvein-website;
      golomb-rice = ./golomb-rice;
      reflex-dom-canvas = ./reflex-dom-canvas;
      reflex-dom-retractable = ./retractable;
      reflex-external-ref = ./reflex-external-ref;
      reflex-flunky = ./reflex-flunky;
      reflex-fork = ./reflex-fork;
      reflex-localize = ./reflex-localize;
      reflex-localize-dom = ./reflex-localize-dom;
      socket-manager = ./socket-manager;
      reflex-main-thread = ./reflex-main-thread;
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
        "ergvein-checkpoint-generator"
        "ergvein-common"
        "ergvein-core"
        "ergvein-crypto"
        "ergvein-filters"
        "ergvein-index-api"
        "ergvein-index-client"
        "ergvein-index-protocol-client"
        "ergvein-index-protocol"
        "ergvein-index-server"
        "ergvein-localize"
        "ergvein-node-discovery"
        "ergvein-types"
        "ergvein-wallet-version"
        "ergvein-wallet"
        "ergvein-website"
        "golomb-rice"
        "reflex-dom-retractable"
        "reflex-external-ref"
        "reflex-flunky"
        "reflex-fork"
        "reflex-localize-dom"
        "reflex-localize"
        "socket-manager"
        "reflex-main-thread"
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
        storeFile = releaseKeyStore;
        storePassword = readPassword releasePasswordFile;
        keyAlias = "ergvein_releasekey";
        keyPassword = readPassword releasePasswordFile;
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

    android.sepulcas = {
      executableName = "sepulcas-demo";
      applicationId = "org.sepulcas.demo";
      displayName = "Sepulcas demo";
      resources = ./sepulcas/static/res;
      assets = ./sepulcas/static/assets;
      iconPath = "@drawable/ic_launcher";
      javaSources = [
        "${project.ghc.sepulcas-android.src}/java"
        "${project.ghc.x509-android.src}/java"
      ];
      version = {code = "1"; name="demo"; };

      services = ''
      <provider
          android:name="androidx.core.content.FileProvider"
          android:authorities="org.sepulcas.demo.fileprovider"
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
