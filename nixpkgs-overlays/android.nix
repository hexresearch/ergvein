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
            # bytestring-0.11.3.0.
            #
            # Tests are disabled because they cause circular dependencies.
            bytestring = hlib.dontBenchmark (hlib.dontCheck (hself.callHackage "bytestring" "0.11.3.0" { tasty-bench = null; }));

            # We have to make sure that our overriden 0.11.3.0 version of bytestring is used
            # instead of the version ? provided by GHC.
            #
            # This is needed because when Nix compiles Haskell packages, it actually passes two
            # different GHC package databases to Cabal.  The first package database contains
            # all the Haskell packages that are distributed with GHC, like bytestring, binary,
            # text, etc.  The second package database actually contains the Nix-built Haskell
            # dependencies, like conduit, lens, etc.
            #
            # By default, GHC (Cabal?) picks the highest version of a library it has.  So, for instance,
            # if we try to pass the bytestring-0.11.3.0 library in the second package database,
            # but bytestring-? is already in the first package database, GHC will always
            # pick bytestring-?, even though we have explicitly overridden the bytestring
            # package with 0.11.3.0 in Nix.
            #
            # To work around this, we pass a constraint to Cabal that tells it to force
            # bytestring-0.11.3.0 even though there is a later version of bytestring
            # available.
            #
            # I don't think this would be required if you wanted to go the other way (e.g. use
            # bytestring-0.10.8.3 instead of bytestring-?).
            mkDerivation = drv: (hsuper.mkDerivation drv).override {
                configureFlags = [ "--constraint=bytestring==0.11.3.0" ];
            };

            # The following are all GHC core libraries.  We need to build them from
            # Hackage so that they have a dependency on our bytestring-0.11.3.0.
            binary = hlib.dontCheck (hself.callHackage "binary" "0.8.6.0" {});
            directory = hself.callHackage "directory" "1.3.3.0" {};
            ghc-boot = hself.callHackage "ghc-boot" "8.6.5" {};
            ghci = hself.callHackage "ghci" "8.6.5" {};
            hpc = hself.callHackage "hpc" "0.6.0.3" {};
            parsec = hself.callHackage "parsec" "3.1.15.0" {};
            process = hself.callHackage "process" "1.6.5.0" {};
            text = hlib.dontBenchmark(hlib.dontCheck (hself.callHackage "text" "1.2.5.0" { tasty-inspection-testing = null; tasty-bench = null; }));
            unix = hself.callHackage "unix" "2.7.2.2" {};

            # Other libraries that depends on old bytestring
            aeson = hlib.dontCheck (hself.callHackage "aeson" "1.5.6.0" {});
            asn1-encoding = hself.callHackage "asn1-encoding" "0.9.6" {};
            blaze-builder = hself.callHackage "blaze-builder" "0.4.2.2" {};
            blaze-markup = hlib.dontCheck (hself.callHackage "blaze-markup" "0.8.2.8" {});
            bytestring-handle = hself.callHackage "bytestring-handle" "0.1.0.6" {};
            bytestring-trie = hself.callHackage "bytestring-trie" "0.2.7" {};
            charset = hself.callHackage "charset" "0.3.9" { };
            ed25519 = hself.callHackage "ed25519" "0.0.5.0" {};
            haddock-library = hself.callHackage "haddock-library" "1.10.0" {};
            hashable = hself.callHackage "hashable" "1.4.0.2" { };
            hlint = hlib.dontCheck (hself.callHackage "hlint" "3.3.6" {});
            HsYAML = hself.callHackage "HsYAML" "0.2.1.0" { };
            http-media = hself.callHackage "http-media" "0.8.0.0" {};
            lukko = hlib.dontCheck (hself.callHackage "lukko" "0.1.1.3" {});
            microlens = hself.callHackage "microlens" "0.4.12.0" {};
            microlens-ghc = hself.callHackage "microlens-ghc" "0.4.13.1" {};
            network = hself.callHackage "network" "3.1.2.7" {};
            optparse-applicative = hself.callHackage "optparse-applicative" "0.17.0.0" {};
            optparse-generic = hself.callHackage "optparse-generic" "1.4.7" {};
            polyparse = hself.callHackage "polyparse" "1.13" {};
            regex-base = hself.callHackage "regex-base" "0.94.0.2" {};
            regex-pcre-builtin = hself.callHackage "regex-pcre-builtin" "0.95.2.3.8.44" {};
            regex-posix = hself.callHackage "regex-posix" "0.96.0.1" {};
            regex-posix-clib = hself.callHackage "regex-posix-clib" "2.7" {};
            regex-tdfa = hself.callHackage "regex-tdfa" "1.3.1.2" { };
            resolv = hlib.dontCheck (hself.callHackage "resolv" "0.1.2.0" {});
            tar = hself.callHackage "tar" "0.5.1.1" {};
            tasty = hself.callHackage "tasty" "1.4.2.1" {};
            tasty-hspec = hself.callHackage "tasty-hspec" "1.2" {};
            text-short = hself.callHackage "text-short" "0.1.5" {};
            time-compat = hlib.dontCheck (hself.callHackage "time-compat" "1.9.6.1" {});
            unicode-data = hself.callHackage "unicode-data" "0.3.0" {};
            unicode-transforms = hself.callHackage "unicode-transforms" "0.4.0.1" {};
            zlib = hself.callHackage "zlib" "0.6.2.3" { zlib = self.zlib; };
            integer-simple = hself.callHackage "integer-simple" "0.1.1.1" {};
            base-orphans = hself.callHackage "base-orphans" "0.8.6" {};
            jailbreak-cabal = hself.callHackage "jailbreak-cabal" "1.3.5" {};
            bytes = hself.callHackage "bytes" "0.17.1" {};
            reducers = hself.callHackage "reducers" "3.12.4" {};
            hashtables = hself.callHackage "hashtables" "1.3" {};
            gtk2hs-buildtools = hself.callHackage "gtk2hs-buildtools" "0.13.8.2" {};

            # The Cabal library is a GHC core library, but it is somewhat special because
            # it needs to be forced to link to the correct version of bytestring when
            # linking the Setup.hs file.
            Cabal = hlib.overrideCabal (hself.callHackage "Cabal" "3.6.3.0" {}) (oldAttrs: {
                preCompileBuildDriver = ''
                    setupCompileFlags="$setupCompileFlags -package bytestring-0.11.3.0"
                '';
            });

            # Failed to configure doctest
            doctest = null;
            cabal-doctest = null;
            network-byte-order = hlib.dontCheck hsuper.network-byte-order;
            distributive = hlib.dontCheck hsuper.distributive;
            iproute = hlib.dontCheck hsuper.iproute;
            prometheus-client = hlib.dontCheck hsuper.prometheus-client;
            prometheus-metrics-ghc = hlib.dontCheck hsuper.prometheus-metrics-ghc;
            semigroupoids = hlib.dontCheck hsuper.semigroupoids;
            generic-data = hlib.dontCheck hsuper.generic-data;
            ghc-lib-parser = hlib.doJailbreak hsuper.ghc-lib-parser;

            # doctest needs to be linked to the ghc Haskell package.  We are not able
            # to override the ghc package with a call to `callHackage` like we do above
            # because the `ghc` attribute actually becomes the compiler.
            #
            # That is to say, `haskell.packages.ghc865.ghc` is a derivation for the GHC
            # compiler, not the ghc Haskell package.
            # doctest = hsuper.doctest.override {
            #     ghc = hself.ghc_8_6_5;
            # };
            # ghc_8_6_5 = hlib.enableCabalFlag (hself.callHackage "ghc" "8.6.5" {}) "buildable";
        });

        };
    };
  };
}
