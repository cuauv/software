#!/bin/bash

export LIBRARY_PATH=`pwd`/../../link-stage

ocamlbuild -quiet -use-ocamlfind -lflags -cclib,-lshm CUAUV_shm.cma
ocamlbuild -quiet -use-ocamlfind -lflags -cclib,-lshm CUAUV_shm.cmxa
