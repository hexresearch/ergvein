set -xe
cabal new-build lib:ergvein-index-server --enable-profiling --ghc-options="-rtsopts"
cabal new-build exe:ergvein-index-server --enable-profiling --ghc-options="-rtsopts"
../dist-newstyle/build/x86_64-linux/ghc-8.6.5/ergvein-index-server-0.1.0.0/x/ergvein-index-server/build/ergvein-index-server/ergvein-index-server --no-drop-filters listen ./configuration.yaml +RTS -p
