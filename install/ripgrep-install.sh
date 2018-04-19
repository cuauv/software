#!/usr/bin/env bash
set -xeuo pipefail

VERSION=0.7.1

tmpDir="$(mktemp -d)"
pushd .
cd "$tmpDir"

archiveName="ripgrep-$VERSION-x86_64-unknown-linux-musl"

wget "https://github.com/BurntSushi/ripgrep/releases/download/$VERSION/$archiveName.tar.gz"
tar -xf "$archiveName".tar.gz
cp "$archiveName"/rg /usr/local/bin

popd
rm -rf "$tmpDir"