
#!/usr/bin/env bash
set -erv

PUBLISH="true"

GIT_HASH=$(git rev-parse HEAD)
GIT_TAG=$(git tag -l --points-at HEAD)
if [ ! -z $GIT_TAG ]; then
  CONTAINER_TAG=$GIT_TAG
  GIT_TAG_ARG="--arg gitTag \"\\\"$GIT_TAG\\\"\""
  PUBLISH="true"
  echo "Publish with $GIT_TAG"
else 
  if [[ "$GIT_BRANCH" == "master" && "$TRAVIS_PULL_REQUEST" == "false" ]]; then
    CONTAINER_TAG="latest"
    PUBLISH="true"
    echo "Publish with latest"
  fi
fi
GIT_BRANCH=$(git branch | grep \* | cut -d ' ' -f2)

containers=$(NIX_PATH=$GIT_NIX_PATH$NIX_PATH nix-build containers.nix \
  --arg containerTag \"$CONTAINER_TAG\" \
  --arg gitHash "\"$GIT_HASH\"" \
  $GIT_TAG_ARG \
  --arg gitBranch "\"$GIT_BRANCH\"" \
  )

for container in $containers
do
  docker load < $container
  docker tag $container ergvein/$container:$CONTAINER_TAG
done

if $PUBLISH; then
docker login --password $DOCKER_PASSWORD --username $DOCKER_USERNAME 
docker push ergvein/ergvein-index-server:$CONTAINER_TAG 
fi