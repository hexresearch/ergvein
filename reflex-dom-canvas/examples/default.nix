{ reflex-platform ? import ../nix/reflex-platform.nix
, compiler ? "ghcjs"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;

  haskellPackages = reflex-platform.${compiler}.override {
    overrides = (self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
      reflex-dom-canvas = self.callPackage ../reflex-dom-canvas.nix {};
    });
  };

  adjust-for-ghcjs = drv: {
    executableToolDepends = [pkgs.closurecompiler pkgs.zopfli];
    doHaddock = false;
    postInstall = ''
      mkdir -p $out

      mkdir -p $out/js
      cp $out/bin/examples.jsexe/all.js $out/js/examples.js

      mkdir -p $out/css
      ln -s ./css/* $out/css

      ln -s ./index.html $out/index.html

      cd $out/bin/examples.jsexe

      closure-compiler all.js \
        --compilation_level=ADVANCED_OPTIMIZATIONS \
        --isolation_mode=IIFE \
        --assume_function_wrapper \
        --jscomp_off="*" \
        --externs=all.js.externs \
        > $out/js/examples.min.js

      rm -Rf $out/bin/examples.jsexe
      rm -Rf $out/bin

      cd $out/js
      zopfli -i1000 examples.min.js

      rm -Rf $out/lib
      rm -Rf $out/nix-support
      rm -Rf $out/share
    '';
  };

  basics = pkgs.haskell.lib.overrideCabal (
    haskellPackages.callPackage ./examples.nix {}
  ) adjust-for-ghcjs;

in
  basics
