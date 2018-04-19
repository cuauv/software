#!/usr/bin/env bash
set -xeuo pipefail

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
