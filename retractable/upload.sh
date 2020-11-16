set -xe
rm ../dist-newstyle/sdist -rf | true
cabal new-sdist
cabal upload --publish ../dist-newstyle/sdist/reflex-dom-retractable-*.tar.gz
