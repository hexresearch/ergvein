set -xe
gitHash=$(git rev-parse --short HEAD)
releaseArgs="--arg gitHash "\"$gitHash\"" -A android.ergvein-wallet --arg release true --arg signApk false"
nix-build $releaseArgs -o android-release "$@"
nix-build $releaseArgs -o android-release-apk --arg releaseBundle false "$@"
