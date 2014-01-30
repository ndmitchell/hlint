#!/bin/bash -e -x
wget https://raw.github.com/ndmitchell/neil/master/travis.sh -O - --no-check-certificate --no-cache --quiet | sh
cabal install
hlint_datadir=data hlint --test
