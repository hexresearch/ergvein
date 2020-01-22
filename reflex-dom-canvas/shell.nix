{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc"
}:
let
  inherit (nixpkgs) pkgs;
  reflex-platform = import ./nix/reflex-platform.nix;
  drv = import ./. { inherit reflex-platform compiler; };
  drvWithTools = pkgs.haskell.lib.addBuildDepends drv
    [ pkgs.cabal-install
      pkgs.haskellPackages.ghcid
    ];
in
  drvWithTools.env
