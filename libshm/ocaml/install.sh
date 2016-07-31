#!/bin/bash

PKGNAME=cuauv-shm

if [[ -z $(ocamlfind query $PKGNAME 2>&1 | grep "not found") ]]; then
    ocamlfind remove $PKGNAME
fi

ocamlfind install $PKGNAME META \
    _build/*.cmi _build/*.cma _build/*.cmxa _build/*.mli _build/*.ml _build/*.a
