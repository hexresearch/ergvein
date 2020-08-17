gitHash=$(git rev-parse --short HEAD)
nix-build -A ghc.ergvein-wallet --arg gitHash "\"$gitHash\""
