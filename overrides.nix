# Here you can put overrides of dependencies
{ reflex-platform
, gitHash
, versionTag
}:
let
  pkgs = reflex-platform.nixpkgs;
  overrideCabal = pkgs.haskell.lib.overrideCabal;
  enableCabalFlag = pkgs.haskell.lib.enableCabalFlag;
  disableCabalFlag = pkgs.haskell.lib.disableCabalFlag;
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
      /android-release
      /android-result
    '';
    in { src = gitignore.gitignoreSourceAux ignore-list pkg.src; } );
  addVersions = drv: pkgs.haskell.lib.overrideCabal drv (drv: {
    preConfigure = (drv.preConfigure or "") + ''
      ${if gitHash == null then "" else "export GIT_HASH=${gitHash}"}
      export VERSION_TAG=${versionTag}
    '';
  });
in (self: super: let
  # Internal packages (depends on production or dev environment)
  callInternal = name: path: args: (
    dontHaddock ( self.callCabal2nix name (ingnoreGarbage path) args ));
  isAndroid = self.ghc.stdenv.targetPlatform.isAndroid;
  walletOpts = if isAndroid then "-fandroid --no-haddock" else "--no-haddock";
  dontProfile = drv: disableCabalFlag drv "profile-reflex";
  in {
    # Internal
    cbitstream = ingnoreGarbage super.cbitstream;
    data-merkle-tree = ingnoreGarbage super.data-merkle-tree;
    ergo-api = lib.dontCheck (ingnoreGarbage super.ergo-api);
    ergvein-checkpoint-generator = ingnoreGarbage super.ergvein-checkpoint-generator;
    ergvein-common = ingnoreGarbage super.ergvein-common;
    ergvein-core = ingnoreGarbage (super.callCabal2nixWithOptions "ergvein-core" ./ergvein-core walletOpts {});
    ergvein-crypto = ingnoreGarbage super.ergvein-crypto;
    ergvein-index-api = ingnoreGarbage super.ergvein-index-api;
    ergvein-index-client = ingnoreGarbage super.ergvein-index-client;
    ergvein-index-protocol = ingnoreGarbage super.ergvein-index-protocol;
    ergvein-index-protocol-client = ingnoreGarbage super.ergvein-index-protocol-client;
    ergvein-index-server = ingnoreGarbage super.ergvein-index-server;
    ergvein-interface-ergo = ingnoreGarbage super.ergvein-interface-ergo;
    ergvein-node-discovery = ingnoreGarbage super.ergvein-node-discovery;
    ergvein-wallet = addVersions (ingnoreGarbage (super.callCabal2nixWithOptions "ergvein-wallet" ./wallet walletOpts {}));
    ergvein-filters = ingnoreGarbage super.ergvein-filters;
    ergvein-types = ingnoreGarbage super.ergvein-types;
    ergvein-wallet-version = ingnoreGarbage super.ergvein-wallet-version;
    golomb-rice = ingnoreGarbage super.golomb-rice;
    reflex-dom-retractable = ingnoreGarbage super.reflex-dom-retractable;
    reflex-external-ref = ingnoreGarbage super.reflex-external-ref;
    reflex-flunky = ingnoreGarbage super.reflex-flunky;
    reflex-fork = ingnoreGarbage super.reflex-fork;
    reflex-localize = ingnoreGarbage super.reflex-localize;
    reflex-localize-dom = ingnoreGarbage super.reflex-localize-dom;
    reflex-main-thread = ingnoreGarbage super.reflex-main-thread;
    sepulcas = ingnoreGarbage (super.callCabal2nixWithOptions "sepulcas" ./sepulcas walletOpts {});
    sepulcas-android = ingnoreGarbage (super.callCabal2nixWithOptions "sepulcas-android" ./sepulcas-android walletOpts {});
    sepulcas-desktop = ingnoreGarbage super.sepulcas-desktop;
    sepulcas-log = ingnoreGarbage super.sepulcas-log;
    sepulcas-native = ingnoreGarbage super.sepulcas-native;
    # Overrides
    android-activity = self.callPackage ./derivations/android-activity.nix {
      inherit (pkgs.buildPackages) jdk;
    };
    bitcoin-api = self.callPackage ./derivations/haskell-bitcoin-api.nix {};
    bitstream = self.callPackage ./derivations/bitstream.nix { };
    byte-order = self.callPackage ./derivations/byte-order.nix {};
    bytestring-trie = self.callPackage ./derivations/bytestring-trie.nix {};
    clay = self.callPackage ./derivations/clay.nix {};
    criterion = lib.dontCheck super.criterion;
    cryptonite = self.callPackage ./derivations/cryptonite.nix {};
    flat = lib.dontCheck (super.flat);
    haskey = self.callPackage ./derivations/haskey.nix {};
    haskoin-core = self.callPackage ./derivations/haskoin-core.nix {};
    hp2any-core = self.callPackage ./derivations/hp2any-core.nix {};
    hp2any-graph = self.callPackage ./derivations/hp2any-graph.nix {};
    immortal-worker = self.callPackage ./derivations/immortal-worker.nix {};
    iproute = self.callPackage ./derivations/iproute.nix {};
    lmdb = self.callPackage ./derivations/haskell-lmdb.nix {};
    parseargs = lib.dontCheck super.parseargs;
    persistent-pagination = self.callPackage ./derivations/persistent-pagination.nix {};
    primitive-unaligned = self.callPackage ./derivations/primitive-unaligned.nix {};
    reflex = enableCabalFlag super.reflex "O2";
    reflex-dom-core = dontProfile (lib.dontCheck (super.reflex-dom-core));
    secp256k1-haskell = self.callPackage ./derivations/secp256k1-haskell.nix {};
    stm-hamt = self.callPackage ./derivations/stm-hamt.nix {};
    tls = lib.dontCheck super.tls;
    wide-word = lib.dontCheck (self.callPackage ./derivations/wide-word.nix { });
    x509-android = super.callCabal2nixWithOptions "x509-android" ./x509-android walletOpts {};
    x509-validation = lib.dontCheck super.x509-validation;
    zlib = self.callPackage ./derivations/zlib.nix {};
    pandoc = self.callPackage ./derivations/pandoc.nix {};
    pandoc-types = self.callPackage ./derivations/pandoc-types.nix {};
    texmath = self.callPackage ./derivations/texmath.nix {};
    HsYAML = self.callPackage ./derivations/HsYAML.nix {};
    doctemplates = self.callPackage ./derivations/doctemplates.nix {};
    haddock-library = self.callPackage ./derivations/haddock-library.nix {};
    hslua = self.callPackage ./derivations/hslua.nix {};
    skylighting = self.callPackage ./derivations/skylighting.nix {};
    skylighting-core = self.callPackage ./derivations/skylighting-core.nix {};
    pandoc-citeproc = self.callPackage ./derivations/pandoc-citeproc.nix {};
  }
)
