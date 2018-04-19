#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

PKGNAME=cuauv-shm

if [[ -z $(ocamlfind query $PKGNAME 2>&1 | grep "not found") ]]; then
    ocamlfind remove -destdir $DIR $PKGNAME
fi

ocamlfind install -destdir $DIR $PKGNAME META \
    _build/*.cmi _build/*.cma _build/*.cmxa _build/*.mli _build/*.ml _build/*.a
