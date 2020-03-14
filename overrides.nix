# Here you can put overrides of dependencies
{ reflex-platform }:
let
  pkgs = reflex-platform.nixpkgs;
  overrideCabal = pkgs.haskell.lib.overrideCabal;
  enableCabalFlag = pkgs.haskell.lib.enableCabalFlag;
  doJailbreak = pkgs.haskell.lib.doJailbreak;
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
  isAndroid = self.ghc.stdenv.targetPlatform.isAndroid;
  walletOpts = if isAndroid then "-fandroid --no-haddock" else "--no-haddock";
  in {
    # Internal
    data-merkle-tree = ingnoreGarbage super.data-merkle-tree;
    ergvein-checkpoint-generator = ingnoreGarbage super.ergvein-checkpoint-generator;
    ergvein-common = ingnoreGarbage super.ergvein-common;
    ergvein-crypto = ingnoreGarbage super.ergvein-crypto;
    ergvein-index-api = ingnoreGarbage super.ergvein-index-api;
    ergvein-index-client = ingnoreGarbage super.ergvein-index-client;
    ergvein-index-server = ingnoreGarbage super.ergvein-index-server;
    ergvein-interface-ergo = ingnoreGarbage super.ergvein-interface-ergo;
    ergvein-wallet = ingnoreGarbage (super.callCabal2nixWithOptions "ergvein-wallet" ./wallet walletOpts {});
    ergvein-wallet-android = ingnoreGarbage (super.callCabal2nixWithOptions "ergvein-wallet-android" ./wallet-android walletOpts {});
    ergvein-wallet-desktop = ingnoreGarbage super.ergvein-wallet-desktop;
    ergvein-wallet-filters = ingnoreGarbage super.ergvein-wallet-filters;
    ergvein-wallet-native = ingnoreGarbage super.ergvein-wallet-native;
    ergvein-wallet-types = ingnoreGarbage super.ergvein-wallet-types;
    ergo-api = lib.dontCheck (ingnoreGarbage super.ergo-api);
    golomb-rice = ingnoreGarbage super.golomb-rice;
    reflex-dom-retractable = ingnoreGarbage super.reflex-dom-retractable;
    reflex-external-ref = ingnoreGarbage super.reflex-external-ref;
    reflex-localize = ingnoreGarbage super.reflex-localize;
    # Overridess
    clay = self.callPackage ./derivations/clay.nix {};
    cryptonite = self.callPackage ./derivations/cryptonite.nix {};
    criterion = lib.dontCheck super.criterion;
    haskoin-core = self.callPackage ./derivations/haskoin-core.nix {};
    bitcoin-api = self.callPackage ./derivations/haskell-bitcoin-api.nix {};
    bytestring-trie = self.callPackage ./derivations/bytestring-trie.nix {};
    zlib = self.callPackage ./derivations/zlib.nix { };
    stm-hamt = self.callPackage ./derivations/stm-hamt.nix { };
    haskey = self.callPackage ./derivations/haskey.nix { };
    persistent-pagination = self.callPackage ./derivations/persistent-pagination.nix {};
    flat = lib.dontCheck (super.flat);
    reflex-dom-core = lib.dontCheck (super.reflex-dom-core);
    bitstream = self.callPackage ./derivations/bitstream.nix { };
    wide-word = lib.dontCheck (self.callPackage ./derivations/wide-word.nix { });
    byte-order = self.callPackage ./derivations/byte-order.nix {};
    primitive-unaligned = self.callPackage ./derivations/primitive-unaligned.nix { };
    lmdb = self.callPackage ./derivations/haskell-lmdb.nix { };
    x509-validation = lib.dontCheck super.x509-validation;
    tls = lib.dontCheck super.tls;
  }
)
