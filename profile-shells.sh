gitHash=$(git rev-parse --short HEAD)
nix-shell --arg gitHash "\"$gitHash\"" -A shells.ghc "$@" --arg profile true
