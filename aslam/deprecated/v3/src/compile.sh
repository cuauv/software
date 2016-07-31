#!/bin/sh

alias ghc="ghc -ferror-spans -W -fspec-constr -threaded -rtsopts -optl-lstdc++ -XOverloadedStrings -XTemplateHaskell -XNoMonomorphismRestriction -XFlexibleContexts"

ghc LoadTemplates.hs -o /dev/null
ghc Main.hs -o main
ghc Visualization.hs -o visualization

rm *.hi *.o *.dyn_hi *.dyn_o

for d in "Symbolic" # "Templating"
do
  cd $d
  rm *.hi *.o *.dyn_hi *.dyn_o
  cd ..
done
