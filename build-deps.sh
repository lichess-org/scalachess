#!/bin/sh
set -e

dir=$(mktemp -d)
echo "Building in $dir"
cd "$dir"

rm -rf scalalib
git clone https://github.com/ornicar/scalalib
cd scalalib
sbt publish-local
cd ..

rm -rf "$dir"
