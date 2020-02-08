
#!/usr/bin/env bash
set -ergv

# Configure cache
source ./setup_git.sh
export NIX_PATH=$GIT_NIX_PATH$NIX_PATH

GIT_HASH=$(git rev-parse HEAD)
tag=$(git tag -l --points-at HEAD)
if [ ! -z $tag ]; then
  GIT_TAG_ARG="--arg gitTag \"\\\"$tag\\\"\""
fi
GIT_BRANCH=$(git branch | grep \* | cut -d ' ' -f2)

containers=$(NIX_PATH=$GIT_NIX_PATH$NIX_PATH nix-build containers.nix \
  --arg isProd true \
  --arg containerTag \"develop\" \
  --arg gitHash "\"$GIT_HASH\"" \
  $GIT_TAG_ARG \
  --arg gitBranch "\"$GIT_BRANCH\"" \
  )
for container in $containers
do
  docker load < $container
done