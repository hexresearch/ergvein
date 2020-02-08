#!/usr/bin/env bash

set -e

# Configure cache
./setup-cache.sh

# Configure CI build
. ./setup-ci-build.sh

export NIX_PATH=$GIT_NIX_PATH$NIX_PATH

containers=$(nix-build containers.nix --arg isProd true \
  --arg isProfile $isProfile \
  --arg containerTag \"$tag\" \
  --arg prefixName \"registry.hxr.team/\" \
  --arg gitHash "\"$gitcommit\"" \
  $gitTagArg \
  --arg gitBranch "\"$gitbranch\"" \
  --arg buildNumber $buildnumber \
  $hotfixArg
  )

for container in $containers
do
  docker load < $container
done