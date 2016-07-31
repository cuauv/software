#!/bin/bash

#< ASLAM >#

#>> Dependencies

CABAL_PACKAGES="aeson lzma vector-algorithms attoparsec base64-bytestring hashable text uuid nanomsg-haskell ansi-terminal normaldistribution GLUT llvm-general-pure llvm-general optparse-applicative snap-server websockets websockets-snap"

#>> Build Configuration

BUILD_DIR=../build
BIN_DIR=$BUILD_DIR/bin
TMP_DIR=$BUILD_DIR/tmp
SYS_DIR=sys

#>> Compilation

GHC_DEFS=""
GHC_OPTS="-XNoMonomorphismRestriction -XTemplateHaskell -XOverloadedStrings -XBangPatterns -XFlexibleContexts -XFlexibleInstances -XUndecidableInstances -XArrows -XLambdaCase -XMultiWayIf -XMultiParamTypeClasses -XFunctionalDependencies -XDeriveGeneric -XRankNTypes -XExplicitForAll -XTypeOperators -XDefaultSignatures -XDataKinds -XKindSignatures -XScopedTypeVariables -XAllowAmbiguousTypes -XInstanceSigs -XTypeFamilies -XCPP -XGeneralizedNewtypeDeriving $GHC_DEFS"
GHC_FLAGS="-ferror-spans -W -j8 -i../../libshm/haskell"
GHC_BIN=/usr/bin/ghc
STATIC="-static -optl-static -fPIC"
OPT="-O3 -fllvm"
RUN_GHC="$GHC_BIN $GHC_OPTS $GHC_FLAGS -outputdir $TMP_DIR -threaded -rtsopts"
RUN_GHCI="$GHC_BIN $GHC_OPTS $GHC_FLAGS --interactive ../../link-stage/libshm.so" 

#>> Colors

GRAY="\033[1;30m"
CYAN="\033[0;36m"
RED="\033[0;31m"
BLUE="\033[0;34m"
YELLOW="\033[1;33m"
GREEN="\033[0;32m"
ENDCOLOR="\033[0m"

#>> Auxiliary

log () {
  STR="[$CYAN`date -u +"%Y/%m/%d %H:%M:%S UTC"`$ENDCOLOR] ($RED""ASLAM""$ENDCOLOR) $1"
  echo -e $STR
}

invoke () {
  log "$YELLOW$1$ENDCOLOR"
  $1
}

files () {
  find -type f -name "*.hs" | tr '\n' ' '
}

usage () {
  log "Usage: run.sh { repl | build | clean | bootstrap }"
  exit 1
}

ACTION=$1

if [ -z "$ACTION" ]; then
  usage
fi

case $ACTION in
  repl)
    invoke "cd src"
    #FILES=`files`
    #MODULES=`echo $FILES | sed -e 's/\.\///g' | sed -e 's/\.hs//g' | sed -e 's/\//./g'`
    #echo ":load $FILES" > .ghci
    #echo ":module $MODULES" >> .ghci
    invoke "$RUN_GHCI"
    #invoke "rm .ghci"
    invoke "cd .."
  ;;
  build)
    invoke "cd src"
    invoke "mkdir -p $BUILD_DIR $BIN_DIR $TMP_DIR"
    invoke "$RUN_GHC Core/CLI.hs -main-is Core.CLI -o $BIN_DIR/aslam"
    invoke "cd .."
  ;;
  clean)
    invoke "cd src"
    invoke "rm -r $BUILD_DIR"
    invoke "cd .."
  ;;
  bootstrap) 
    if [ -f '/etc/arch-release' ]; then
      log "Detected Arch Linux install."
      invoke "sudo pacman -S ghc cabal-install llvm35 clang35 nanomsg"
      invoke "cabal update"
      invoke "cabal install $CABAL_PACKAGES"
      invoke "./run.sh build"
      log "ASLAM has been installed!"
    else
      log "Only Arch Linux is supported!"
    fi 
  ;;
  *)
    usage
  ;;
esac
