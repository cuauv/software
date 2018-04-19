#!/usr/bin/env bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Fake generation script to plug into ninja.
# Builds and installs.

pushd $DIR >/dev/null

$DIR/build.sh
# ocamlfind really doesn't want to shut up...
$DIR/install.sh 2>/dev/null >/dev/null

popd >/dev/null

# Fake file to manage timestamps.
touch $DIR/install.fake
