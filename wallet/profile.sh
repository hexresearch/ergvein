cabal new-build ergvein --enable-profiling
hp2any-graph -e ../dist-newstyle/build/x86_64-linux/ghc-8.6.5/ergvein-wallet-0.1.0.0/x/ergvein/build/ergvein/ergvein -- +RTS -hc -i0.02 -L256
