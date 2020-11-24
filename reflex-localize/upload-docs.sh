#!/bin/sh
set -xe

rm ../dist-newstyle/reflex-external-ref-*-docs.tar.gz

# assumes cabal 2.4 or later
cabal v2-haddock --haddock-for-hackage --enable-doc

cabal upload -d --publish ../dist-newstyle/reflex-external-ref-*.tar.gz
