# Fish Environment

set -g POOLTEST_DIR (pwd)"/"(dirname $argv)

function log 
  set_color green
  echo -e [(date +"%H:%M:%S %d/%m/%Y")] '\c'
  set_color yellow
  echo -e (pwd) '\c'
  set_color normal 
  echo $argv
end

log "Detected script directory: $POOLTEST_DIR"

function invoke
  function redify
    set_color red 
    echo -e $argv '\c'
    set_color normal
  end 
  set -l STR (redify $argv)
  log $STR
  eval $argv
end

function ghci
  set GHC_OPTS "-XNoMonomorphismRestriction -XTemplateHaskell -XOverloadedStrings -XBangPatterns -XFlexibleContexts -XFlexibleInstances -XUndecidableInstances -XArrows -XLambdaCase -XMultiWayIf -XMultiParamTypeClasses -XFunctionalDependencies -XDeriveGeneric -XRankNTypes -XExplicitForAll -XTypeOperators -XDefaultSignatures -XDataKinds -XKindSignatures -XScopedTypeVariables -XAllowAmbiguousTypes -XInstanceSigs -XTypeFamilies -XGeneralizedNewtypeDeriving -XCPP -XUnicodeSyntax -XMagicHash"
  set GHC_FLAGS "-ferror-spans -Wall -fno-warn-orphans -fno-warn-name-shadowing -i../../libshm/haskell ../../link-stage/libshm.so"
  invoke "pushd"
  invoke "cd $POOLTEST_DIR/$argv"
  invoke "stack exec -- ghci $GHC_OPTS $GHC_FLAGS"
  invoke "popd"
end

function lint
  invoke "hlint $POOLTEST_DIR/src --hint=Default --hint=Generalise"
end
