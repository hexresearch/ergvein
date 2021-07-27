self: super:
let pkgs = super;
in rec {
  android-activity = self.callPackage ../derivations/android-activity.nix {};
  secp256k1Sys = self.callPackage ../derivations/secp256k1Sys.nix {};
  zlibSys = self.callPackage ../derivations/zlibSys.nix {};
  lmdbSys = self.callPackage ../derivations/lmdbSys.nix {};
  leveldb = self.callPackage ../derivations/leveldb.nix {};
  p11-kit = self.callPackage ../derivations/p11-kit.nix {};

  haskell-language-server = self.callPackage ../derivations/haskell-language-server/withWrapper.nix { };
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc865 = super.haskell.packages.ghc865.override { 
        overrides = (self: super: {
          fourmolu = self.callPackage ../derivations/fourmolu.nix { }; 
          dependent-sum = self.callPackage ../derivations/dependent-sum.nix { }; 
          dependent-map = self.callPackage ../derivations/dependent-map.nix { }; 
          hashable = self.callPackage ../derivations/hashable.nix { };
          path = self.callPackage ../derivations/path.nix { };
          hiedb = self.callPackage ../derivations/hiedb.nix { };
          tasty-bench = self.callPackage ../derivations/tasty-bench.nix { };
          HsYAML = self.callPackage ../derivations/HsYAML_2.nix { };
          HsYAML-aeson = self.callPackage ../derivations/HsYAML-aeson.nix { };
          hie-compat = self.callPackage ../derivations/hie-compat.nix { };
          hie-bios = self.callPackage ../derivations/hie-bios.nix { };
          ghcide = self.callPackage ../derivations/ghcide.nix { }; 
          lsp = self.callPackage ../derivations/lsp.nix { }; 
          lsp-types = self.callPackage ../derivations/lsp-types.nix { }; 
          ormolu = self.callPackage ../derivations/ormolu.nix { };
          ghc-trace-events = self.callPackage ../derivations/ghc-trace-events.nix { };
          ghc-check = self.callPackage ../derivations/ghc-check.nix { };
          retrie = self.callPackage ../derivations/retrie.nix { };
          th-compat = self.callPackage ../derivations/th-compat.nix { };
          some = self.callPackage ../derivations/some.nix { };
          shake-bench = self.callPackage ../derivations/shake-bench.nix { };
          opentelemetry = self.callPackage ../derivations/opentelemetry.nix { };
          heapsize = self.callPackage ../derivations/heapsize.nix { };
          implicit-hie = self.callPackage ../derivations/implicit-hie.nix { };
          implicit-hie-cradle = self.callPackage ../derivations/implicit-hie-cradle.nix { };
          hls-brittany-plugin = self.callPackage ../derivations/hls-brittany-plugin.nix { }; 
          hls-class-plugin = self.callPackage ../derivations/hls-class-plugin.nix { }; 
          hls-eval-plugin = self.callPackage ../derivations/hls-eval-plugin.nix { }; 
          hls-hlint-plugin = self.callPackage ../derivations/hls-hlint-plugin.nix { }; 
          hls-plugin-api = self.callPackage ../derivations/hls-plugin-api.nix { }; 
          hls-retrie-plugin = self.callPackage ../derivations/hls-retrie-plugin.nix { };
          hls-splice-plugin = self.callPackage ../derivations/hls-splice-plugin.nix { };
          hls-tactics-plugin = self.callPackage ../derivations/hls-tactics-plugin.nix { };
          hls-test-utils = self.callPackage ../derivations/hls-test-utils.nix { };
          hls-explicit-imports-plugin = self.callPackage ../derivations/hls-explicit-imports-plugin.nix { };
          hls-haddock-comments-plugin = self.callPackage ../derivations/hls-haddock-comments-plugin.nix { };
          hls-stylish-haskell-plugin = self.callPackage ../derivations/hls-stylish-haskell-plugin.nix { };
          haskell-language-server = self.callPackage ../derivations/haskell-language-server/default.nix { }; 
        } );
      };
    };
  };
}
