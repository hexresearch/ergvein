# Here you can put overrides of dependencies
{ reflex-platform ? (import ./reflex-platform.nix {}), ... }:
let
  pkgs = reflex-platform.nixpkgs;
  overrideCabal = pkgs.haskell.lib.overrideCabal;
  lib = pkgs.haskell.lib;
  dontHaddock = lib.dontHaddock;
  gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "siers";
    repo = "nix-gitignore";
    rev = "ce0778ddd8b1f5f92d26480c21706b51b1af9166";
    sha256 = "1d7ab78i2k13lffskb23x8b5h24x7wkdmpvmria1v3wb9pcpkg2w";
  }) {};
  ingnoreGarbage = pkg: overrideCabal pkg (pkg : let
    ignore-list = ''
      /.ghc.environment*
      /dist-newstyle
    '';
    in { src = gitignore.gitignoreSourceAux ignore-list pkg.src; } );

in (self: super: let
  # Internal packages (depends on production or dev environment)
  callInternal = name: path: args: (
    dontHaddock ( self.callCabal2nix name (ingnoreGarbage path) args ));
  in {
    # Internal
    ergvein-common = ingnoreGarbage super.ergvein-common;
    ergvein-index-api = ingnoreGarbage super.ergvein-index-api;
    ergvein-index-server = ingnoreGarbage super.ergvein-index-server;
    ergvein-wallet = ingnoreGarbage super.ergvein-wallet;
    ergvein-crypto = ingnoreGarbage super.ergvein-crypto;
    # Overrides
    clay = self.callPackage ./derivations/clay.nix {};
    cryptonite = self.callPackage ./derivations/cryptonite.nix {};
  }
)
