set -xe
rm ../dist-newstyle/sdist -rf | true
cabal new-sdist
cabal upload --publish ../dist-newstyle/sdist/reflex-flunky-*.tar.gz
