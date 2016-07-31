#!/bin/sh

alias ghc="ghc -ferror-spans -W -Odph -rtsopts -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -optlo-O3 -XOverloadedStrings -XTemplateHaskell -XNoMonomorphismRestriction -XBangPatterns"

ghc Main.hs -o main
ghc Visualization.hs -o visualization

rm *.hi *.o *.dyn_hi *.dyn_o
