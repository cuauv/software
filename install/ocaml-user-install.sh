#!/usr/bin/env bash
set -xeuo pipefail

if [[ "$(uname -m)" != "x86_64" ]]; then
	echo "Cannot install OCaml on $(uname -m)"
	exit 0
fi

export DEBIAN_FRONTEND=noninteractive

packages=(
    # General tools
    jbuilder
    merlin
    ocp-indent
    utop

    # Libraries
    async
    core
    ctypes
    ctypes-foreign
)

opam init
opam switch 4.05.0
eval "$(opam config env)"
opam install "${packages[@]}"
opam user-setup install
