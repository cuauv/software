#!/usr/bin/env bash

export LIBRARY_PATH=`pwd`/../../link-stage

# TODO: this is a temporary fix to address the OCaml compiler not being tail-recursive...
# Since we have too many modules in a single file, the compiler runs into a stack overflow,
# which can only be resolved by increasing the stack limit
# This line should be removed when a future version of the OCaml compiler (>4.05.0) is released.
ulimit -s unlimited

ocamlbuild -quiet -use-ocamlfind -lflags -cclib,-lshm CUAUV_shm.cma
ocamlbuild -quiet -use-ocamlfind -lflags -cclib,-lshm CUAUV_shm.cmxa
