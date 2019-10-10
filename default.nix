{ release ? false }:
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
    ergvein-index-api = ./index-api;
    ergvein-wallet = ./wallet;
    ergvein-crypto = ./crypto;
    reflex-localize = ./reflex-localize;
    reflex-external-ref = ./reflex-external-ref;
  };
  shells = {
    ghc = [
      "ergvein-common"
      "ergvein-index-api"
      "ergvein-wallet"
      "ergvein-crypto"
      "reflex-localize"
      "reflex-external-ref"
    ];
  };
  overrides = import ./overrides.nix { inherit reflex-platform; };

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
