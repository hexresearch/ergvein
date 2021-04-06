gitHash=$(git rev-parse --short HEAD)
nix-build --arg isAndroid true --arg gitHash "\"$gitHash\"" -A android.sepulcas -o android-result "$@" ../default.nix
