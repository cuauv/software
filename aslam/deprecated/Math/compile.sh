#!/bin/sh

alias ghc="ghc -ferror-spans -W -O3 -fstatic-argument-transformation -fspec-constr -threaded -rtsopts -XOverloadedStrings -XTemplateHaskell -XNoMonomorphismRestriction -XBangPatterns"

ghc main.hs -o main

rm *.hi *.o *.dyn_hi *.dyn_o
