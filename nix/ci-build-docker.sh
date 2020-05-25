
#!/usr/bin/env bash
set -erv

GIT_BRANCH=$GITHUB_BRANCH
GIT_HASH=$(git rev-parse HEAD)
GIT_TAG=$(git tag -l --points-at HEAD)
CONTAINER_TAG="$GIT_BRANCH"
PUBLISH="false"
if [ ! -z $GIT_TAG ]; then
  CONTAINER_TAG=$GIT_TAG
  GIT_TAG_ARG="--arg gitTag \"\\\"$GIT_TAG\\\"\""
  PUBLISH="true"
  echo "Publish with $GIT_TAG"
else 
  if [[ "$GIT_BRANCH" == "master" && "$GITHUB_EVENT_NAME" != "pull_request" ]]; then
    CONTAINER_TAG="latest"
    PUBLISH="true"
    echo "Publish with latest"
  fi
fi

echo "Git branch: $GIT_BRANCH"
echo "Container tag: $CONTAINER_TAG"

containers=$(NIX_PATH=$GIT_NIX_PATH$NIX_PATH nix-build containers.nix \
  --arg containerTag \"$CONTAINER_TAG\" \
  --arg gitHash "\"$GIT_HASH\"" \
  $GIT_TAG_ARG \
  --arg gitBranch "\"$GIT_BRANCH\"" \
  )

for container in $containers
do
  echo "Loading $container..."
  docker load < $container
done

if $PUBLISH; then
echo $DOCKER_PASSWORD | docker login --username $DOCKER_USERNAME --password-stdin
docker tag ergvein-index-server:$CONTAINER_TAG ergvein/ergvein-index-server:$CONTAINER_TAG
docker push ergvein/ergvein-index-server:$CONTAINER_TAG 
fi
