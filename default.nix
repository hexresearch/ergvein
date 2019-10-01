{ }:
let
  reflex-platform = import ./reflex-platform.nix {};
in reflex-platform.project ({ pkgs, ... }: {
  packages = {
    ergvein-common = ./common;
    ergvein-wallet = ./wallet;
  };
  shells = {
    ghc = [
      "ergvein-common"
      "ergvein-wallet"
    ];
  };
  overrides = import ./overrides.nix { inherit reflex-platform; };
})
