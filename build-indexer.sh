gitHash=$(git rev-parse --short HEAD)
nix-build -A ghc.ergvein-index-server --arg gitHash "\"$gitHash\""
