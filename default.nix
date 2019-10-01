{ }:
let
  reflex-platform = import ./reflex-platform.nix {};
in reflex-platform.project ({ pkgs, ... }: {
  packages = {

  };
  shells = {
    ghc = [
        "ergvein-index-api"
    ];
  };
  overrides = import ./overrides.nix { inherit reflex-platform; };
})
