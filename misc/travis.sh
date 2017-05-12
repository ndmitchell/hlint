#!/bin/sh
curl -sL https://raw.github.com/ndmitchell/neil/master/misc/travis.sh | sh -s -- hlint $*
