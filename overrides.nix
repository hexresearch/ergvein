# Here you can put overrides of dependencies
{ reflex-platform ? (import ./reflex-platform.nix {}), ... }:
let
  pkgs = reflex-platform.nixpkgs;
  overrideCabal = pkgs.haskell.lib.overrideCabal;
  enableCabalFlag = pkgs.haskell.lib.enableCabalFlag;
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
  isAndroid = self.ghc.stdenv.targetPlatform.libc == "bionic";
  addAndroidFlag = f : p : if isAndroid then enableCabalFlag p f else p;
  androidOverride = drv: if isAndroid then overrideCabal drv (drv: {
    configureFlags = (drv.configureFlags or []) ++ ["-fandroid"];
  }) else drv;
  in {
    # Internal
    ergvein-common = ingnoreGarbage super.ergvein-common;
    ergvein-crypto = ingnoreGarbage super.ergvein-crypto;
    ergvein-index-api = ingnoreGarbage super.ergvein-index-api;
    ergvein-index-server = ingnoreGarbage super.ergvein-index-server;
    ergvein-wallet = androidOverride (ingnoreGarbage super.ergvein-wallet);
    reflex-dom-retractable = ingnoreGarbage super.reflex-dom-retractable;
    reflex-external-ref = ingnoreGarbage super.reflex-external-ref;
    reflex-localize = ingnoreGarbage super.reflex-localize;
    # Overrides
    clay = self.callPackage ./derivations/clay.nix {};
    cryptonite = self.callPackage ./derivations/cryptonite.nix {};
    haskoin-core = self.callPackage ./derivations/haskoin-core.nix {};
    haskell-bitcoin-api = self.callPackage ./derivations/haskell-bitcoin-api.nix {};
  }
)
