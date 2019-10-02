{ release ? false }:
let
  reflex-platform = import ./reflex-platform.nix {};
in reflex-platform.project ({ pkgs, ... }: {
  packages = {
    ergvein-common = ./common;
    ergvein-index-api = ./index-api;
    ergvein-wallet = ./wallet;
  };
  shells = {
    ghc = [
      "ergvein-common"
      "ergvein-index-api"
      "ergvein-wallet"
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
