#!/bin/sh

alias ghc="ghc -ferror-spans -W -Odph -rtsopts -threaded -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -O3 -XOverloadedStrings -XTemplateHaskell -XNoMonomorphismRestriction -XBangPatterns"

ghc API -o api


rm *.hi *.o *.dyn_hi *.dyn_o
