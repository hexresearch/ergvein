gitHash=$(git rev-parse --short HEAD)
nix-build --arg gitHash "\"$gitHash\"" -A android.ergvein-wallet -o android-release --arg release true "$@"
