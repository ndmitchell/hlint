#!/bin/bash -e -x
rm -f dist/*.tar.gz
rm -rf dist/snapshot
cabal configure --ghc-options=-Werror
cabal sdist
cd dist
mkdir snapshot
tar -xf *.tar.gz -C snapshot
cd snapshot
mv hlint-* hlint # predictable name
cd hlint
cabal configure --ghc-options=-Werror
cabal build
cd ../../..
export PATH=/home/travis/.ghc-multi/7.6.3/bin:$PATH
hlint_datadir=data dist/snapshot/hlint/dist/build/hlint/hlint --test
