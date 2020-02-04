nix-build -A ghc.ergvein-wallet | cachix push ergvein
./make-android.sh | cachix push ergvein
