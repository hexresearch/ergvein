{ }:
let
  reflex-platform = import ./reflex-platform.nix {};
in reflex-platform.project ({ pkgs, ... }: {
  packages = {

  };
  shells = {
    ghc = [

    ];
  };
  overrides = import ./overrides.nix { inherit reflex-platform; };
})
