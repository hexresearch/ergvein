self: super:
let
    hlib = super.haskell.lib;
in {
  all-cabal-hashes = self.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/cd020ce0c43633787e44495c573366e522ab405f.tar.gz;
    sha256 = "sha256-jVWN4yaa42Y1BCT7eqfbn1XkecyfBCERHxo4Rb7t8eI=";
  };

  haskell = super.haskell // {
    packages = super.haskell.packages // {
        ghc865 = super.haskell.packages.ghc865.override {
            overrides = (hself: hsuper: {
            # The bytestring version provided by GHC is ?.  This overrides it to use
            # bytestring-0.10.8.2.
            #
            # Tests are disabled because they cause circular dependencies.
            bytestring = hlib.dontBenchmark (hlib.dontCheck (hself.callCabal2nix "bytestring" ../bytestring { }));

            # We have to make sure that our overriden 0.10.8.2 version of bytestring is used
            # instead of the version ? provided by GHC.
            #
            # This is needed because when Nix compiles Haskell packages, it actually passes two
            # different GHC package databases to Cabal.  The first package database contains
            # all the Haskell packages that are distributed with GHC, like bytestring, binary,
            # text, etc.  The second package database actually contains the Nix-built Haskell
            # dependencies, like conduit, lens, etc.
            #
            # By default, GHC (Cabal?) picks the highest version of a library it has.  So, for instance,
            # if we try to pass the bytestring-0.10.8.2 library in the second package database,
            # but bytestring-? is already in the first package database, GHC will always
            # pick bytestring-?, even though we have explicitly overridden the bytestring
            # package with 0.10.8.2 in Nix.
            #
            # To work around this, we pass a constraint to Cabal that tells it to force
            # bytestring-0.10.8.2 even though there is a later version of bytestring
            # available.
            #
            # I don't think this would be required if you wanted to go the other way (e.g. use
            # bytestring-0.10.8.3 instead of bytestring-?).
            mkDerivation = drv: (hsuper.mkDerivation drv).override {
                configureFlags = [ "--constraint=bytestring==0.10.8.2" ];
            };

            # The following are all GHC core libraries.  We need to build them from
            # Hackage so that they have a dependency on our bytestring-0.10.8.2.
            binary = hlib.dontCheck (hself.callHackage "binary" "0.8.6.0" {});
            directory = hself.callHackage "directory" "1.3.3.0" {};
            ghc-boot = hself.callHackage "ghc-boot" "8.6.5" {};
            ghci = hself.callHackage "ghci" "8.6.5" {};
            hpc = hself.callHackage "hpc" "0.6.0.3" {};
            parsec = hself.callHackage "parsec" "3.1.13.0" {};
            process = hself.callHackage "process" "1.6.5.0" {};
            text = hself.callHackage "text" "1.2.3.1" {};
            unix = hself.callHackage "unix" "2.7.2.2" {};
            template-haskell = hself.callHackage "template-haskell" "2.14.0.0" {};

            # Other libraries that depends on old bytestring
            # aeson = hlib.dontCheck (hself.callHackage "aeson" "2.0.3.0" {});
            # asn1-encoding = hself.callHackage "asn1-encoding" "0.9.6" {};
            # monoidal-containers = hlib.dontCheck (hself.callHackage "monoidal-containers" "0.6.2.0" { });
            # async = hself.callHackage "async" "2.2.4" {};
            # haskoin-core = hself.callHackage "haskoin-core" "0.12.0" {};
            # witherable = hself.callHackage "witherable" "0.4.2" {};
            # blaze-builder = hself.callHackage "blaze-builder" "0.4.2.2" {};
            # blaze-markup = hlib.dontCheck (hself.callHackage "blaze-markup" "0.8.2.8" {});
            # bytestring-handle = hself.callHackage "bytestring-handle" "0.1.0.6" {};
            # bytestring-trie = hself.callHackage "bytestring-trie" "0.2.7" {};
            # charset = hself.callHackage "charset" "0.3.9" { };
            # constraints = hself.callHackage "constraints" "0.13.3" { };
            # ed25519 = hself.callHackage "ed25519" "0.0.5.0" {};
            # data-fix = hself.callHackage "data-fix" "0.3.2" {};
            # haddock-library = hself.callHackage "haddock-library" "1.10.0" {};
            # hashable = hlib.dontCheck (hself.callHackage "hashable" "1.4.0.2" {});
            # hlint = hlib.dontCheck (hself.callHackage "hlint" "3.3.6" {});
            # HsYAML = hself.callHackage "HsYAML" "0.2.1.0" { };
            # http-media = hself.callHackage "http-media" "0.8.0.0" {};
            # lukko = hlib.dontCheck (hself.callHackage "lukko" "0.1.1.3" {});
            # microlens = hself.callHackage "microlens" "0.4.12.0" {};
            # microlens-ghc = hself.callHackage "microlens-ghc" "0.4.13.1" {};
            # network = hself.callHackage "network" "3.1.2.7" {};
            # optparse-applicative = hlib.dontCheck (hself.callHackage "optparse-applicative" "0.17.0.0" {});
            # optparse-generic = hself.callHackage "optparse-generic" "1.4.7" {};
            # polyparse = hself.callHackage "polyparse" "1.13" {};
            # regex-base = hself.callHackage "regex-base" "0.94.0.2" {};
            # regex-pcre-builtin = hself.callHackage "regex-pcre-builtin" "0.95.2.3.8.44" {};
            # regex-posix = hself.callHackage "regex-posix" "0.96.0.1" {};
            # regex-posix-clib = hself.callHackage "regex-posix-clib" "2.7" {};
            # regex-tdfa = hself.callHackage "regex-tdfa" "1.3.1.2" { };
            # resolv = hlib.dontCheck (hself.callHackage "resolv" "0.1.2.0" {});
            # tar = hself.callHackage "tar" "0.5.1.1" {};
            # tasty = hself.callHackage "tasty" "1.4.2.1" {};
            # tasty-hspec = hself.callHackage "tasty-hspec" "1.2" {};
            # text-short = hself.callHackage "text-short" "0.1.5" {};
            # vty = hself.callHackage "vty" "5.35.1" {};
            # time-compat = hlib.dontCheck (hself.callHackage "time-compat" "1.9.6.1" {});
            # unordered-containers = hlib.dontCheck (hself.callHackage "unordered-containers" "0.2.17.0" {});
            # unicode-data = hself.callHackage "unicode-data" "0.3.0" {};
            # unicode-transforms = hself.callHackage "unicode-transforms" "0.4.0.1" {};
            # zlib = hself.callHackage "zlib" "0.6.2.3" { zlib = self.zlib; };
            # integer-simple = hself.callHackage "integer-simple" "0.1.1.1" {};
            # base-orphans = hlib.dontCheck (hself.callHackage "base-orphans" "0.8.6" {});
            # jailbreak-cabal = hself.callHackage "jailbreak-cabal" "1.3.5" {};
            # bytes = hself.callHackage "bytes" "0.17.1" {};
            # reducers = hself.callHackage "reducers" "3.12.4" {};
            # hashtables = hlib.dontCheck (hself.callHackage "hashtables" "1.3" {});
            # test-framework = hlib.dontCheck (hself.callHackage "test-framework" "0.8.2.0" {});
            # test-framework-quickcheck2 = hlib.dontCheck (hself.callHackage "test-framework-quickcheck2" "0.3.0.5" {});
            # splitmix = hlib.dontCheck (hself.callHackage "splitmix" "0.1.0.4" { testu01 = null; });
            # glib = hlib.dontCheck (hlib.doJailbreak (hself.callHackage "glib" "0.13.8.1" { glib = self.glib; }));
            # vector = hlib.dontCheck (hself.callHackage "vector" "0.12.3.1" {});
            # gtk2hs-buildtools = hself.callPackage ../derivations/gtk2hs-buildtools.nix {};
            # hedgehog = hself.callHackage "hedgehog" "1.1.1" {};
            # lens-action = hself.callHackage "lens-action" "0.2.6" {};
            # these-lens = hself.callHackage "these-lens" "1.0.1.2" {};
            # tasty-hedgehog = hself.callHackage "tasty-hedgehog" "1.2.0.0" {};
            # tasty-discover = hlib.dontCheck (hself.callHackage "tasty-discover" "4.2.2" {});
            # JuicyPixels = hself.callHackage "JuicyPixels" "3.3.7" {};
            # qrcode-core = hself.callHackage "qrcode-core" "0.9.5" {};
            # safecopy = hself.callHackage "safecopy" "0.10.4.2" {};
            # free = hself.callHackage "free" "5.1.7" {};
            # profunctors = hself.callHackage "profunctors" "5.6.2" {};
            # contravariant = hself.callHackage "contravariant" "1.5.5" {};
            # microlens-platform = hself.callHackage "microlens-platform" "0.4.2.1" {};
            # th-lift-instances = hself.callHackage "th-lift-instances" "0.1.19" {};
            # StateVar = hself.callHackage "StateVar" "1.2.2" {};
            # hasktags = hself.callHackage "hasktags" "0.72.0" {};
            # semigroupoids = hlib.dontCheck (hself.callHackage "semigroupoids" "5.3.7" {});
            # # hslua = hself.callHackage "hslua" "2.2.0" {};
            # hslua-module-text = hself.callHackage "hslua-module-text" "1.0.2" {};
            # hslua-core = hself.callHackage "hslua-core" "2.2.0" {};
            # hslua-packaging = hself.callHackage "hslua-packaging" "2.2.0" {};
            # lua = hself.callHackage "lua" "2.2.0" {};
            # lua-arbitrary = hself.callHackage "lua-arbitrary" "1.0.1" {};
            # bsb-http-chunked = hlib.dontCheck (hself.callHackage "bsb-http-chunked" "0.0.0.4" {});
            # hslua-objectorientation = hself.callHackage "hslua-objectorientation" "2.2.0" {};
            # hslua-marshalling = hself.callHackage "hslua-marshalling" "2.2.0" {};
            # hslua-module-system = hself.callHackage "hslua-module-system" "1.0.2" {};
            # tasty-hslua = hself.callHackage "tasty-hslua" "1.0.2" {};
            # hashable-time = hself.callHackage "hashable-time" "0.3" {};
            # lifted-async = hself.callHackage "lifted-async" "0.10.2.2" {};
            # bitstream = hlib.doJailbreak hsuper.bitstream;
            # http-date = hlib.dontCheck (hself.callHackage "http-date" "0.0.11" {});
            # prettyprinter = hlib.dontCheck (hself.callHackage "prettyprinter" "1.7.1" {});
            # prettyprinter-ansi-terminal = hlib.dontCheck (hself.callHackage "prettyprinter-ansi-terminal" "1.1.3" {});
            # these = hself.callHackage "these" "1.1.1.1" {};
            # vault = hself.callHackage "vault" "0.3.1.5" {};
            # semialign = hself.callHackage "semialign" "1.2.0.1" {};
            # rebase = hlib.doJailbreak (hself.callHackage "rebase" "1.15.0.3" {});
            # rerebase = hlib.doJailbreak (hself.callHackage "rerebase" "1.15.0.3" {});
            # keys = hself.callHackage "keys" "3.12.3" {};
            # tasty-lua = hself.callHackage "tasty-lua" "1.0.2" {};
            # pointed = hself.callHackage "pointed" "5.0.3" {};
            # groups = hself.callHackage "groups" "0.5.3" {};
            # selective = hself.callHackage "selective" "0.5" {};
            # tasty-inspection-testing = hself.callHackage "tasty-inspection-testing" "0.1" {};
            # tasty-bench = hself.callHackage "tasty-bench" "0.1" {};
            # validation = hlib.doJailbreak (hself.callHackage "validation" "1.1.2" {});
            # trifecta = hself.callHackage "trifecta" "2.1.2" {};
            # turtle = hlib.dontCheck (hself.callHackage "turtle" "1.5.24" {});
            # OneTuple = hself.callHackage "OneTuple" "0.3.1" {};
            # random = hlib.dontBenchmark (hlib.dontCheck (hself.callHackage "random" "1.2.1" {}));
            # QuickCheck = hself.callHackage "QuickCheck" "2.14.2" { };
            # template-haskell = hself.callHackage "template-haskell" "2.18.0.0" { };
            # ghc-boot-th = hself.callHackage "ghc-boot-th" "9.2.1" { };
            # foldl = hlib.dontCheck (hself.callHackage "foldl" "1.4.12" {});


            # The Cabal library is a GHC core library, but it is somewhat special because
            # it needs to be forced to link to the correct version of bytestring when
            # linking the Setup.hs file.
            Cabal = hlib.overrideCabal (hself.callHackage "Cabal" "2.4.0.1" {}) (oldAttrs: {
                preCompileBuildDriver = ''
                    setupCompileFlags="$setupCompileFlags -package bytestring-0.10.8.2"
                '';
            });

            # Failed to configure doctest
            # doctest = null;
            # cabal-doctest = null;
            # network-byte-order = hlib.dontCheck hsuper.network-byte-order;
            # distributive = hlib.dontCheck hsuper.distributive;
            # iproute = hlib.dontCheck hsuper.iproute;
            # prometheus-client = hlib.dontCheck hsuper.prometheus-client;
            # prometheus-metrics-ghc = hlib.dontCheck hsuper.prometheus-metrics-ghc;
            # generic-data = hlib.dontCheck hsuper.generic-data;
            # ghc-lib-parser = hlib.doJailbreak hsuper.ghc-lib-parser;
            # http-types = hlib.dontCheck hsuper.http-types;
            # pgp-wordlist = hlib.dontCheck hsuper.pgp-wordlist;
            # wai-middleware-prometheus = hlib.dontCheck hsuper.wai-middleware-prometheus;
            
            # doctest needs to be linked to the ghc Haskell package.  We are not able
            # to override the ghc package with a call to `callHackage` like we do above
            # because the `ghc` attribute actually becomes the compiler.
            #
            # That is to say, `haskell.packages.ghc865.ghc` is a derivation for the GHC
            # compiler, not the ghc Haskell package.
            doctest = hsuper.doctest.override {
              ghc = hself.ghc_8_6_5;
            };
            ghc-boot-th = hself.callHackage "ghc-boot-th" "8.6.5" {};
            ghc_8_6_5 = hlib.enableCabalFlag (hself.callPackage ../derivations/ghc_8_6_5.nix { unbuildable = null; }) "buildable";        
          });
        };
    };
  };
}
