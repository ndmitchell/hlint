#!/bin/sh
VERSION=2.0.6
TEMP=$(mktemp --directory .hlint-XXXXX)
mkdir $TEMP
curl -o$TEMP/hlint.tar.gz -L --insecure https://github.com/ndmitchell/hlint/releases/download/v$VERSION/hlint-$VERSION-x86_64-linux.tar.gz
tar -xzf $TEMP/hlint.tar.gz -C$TEMP
$TEMP/hlint-$VERSION/hlint $*
