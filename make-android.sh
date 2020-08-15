gitHash=$(git rev-parse --short HEAD)
nix-build --arg isAndroid true --arg gitHash "\"$gitHash\"" -A android.ergvein-wallet -o android-result "$@"
