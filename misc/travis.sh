#!/bin/sh
curl --location --silent https://raw.github.com/ndmitchell/neil/master/misc/travis.sh | sh -s -- hlint $*
