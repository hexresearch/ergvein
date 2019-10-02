{ }:
let
  reflex-platform = import ./reflex-platform.nix {};
in reflex-platform.project ({ pkgs, ... }: {
  packages = {
    ergvein-common = ./common;
    ergvein-index-api = ./ergvein-index-api;
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
})
