#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/.. && pwd )"

cat <<end > pooltest/pooltest.cabal
name:           pooltest
version:        0.1.0.0
synopsis:       Pool Testing Scripts
license:        BSD3
author:         Christopher Goes
maintainer:     cwgoes@protonmail.ch
copyright:      2016 CUAUV
build-type:     Simple
cabal-version:  >=1.10

executable pooltest
  hs-source-dirs:       src
                        ../libshm/haskell
  main-is:              CLI.hs
  build-depends:        base,
                        text,
                        aeson,
                        lens,
                        binary,
                        directory,
                        unix,
                        process,
                        lens-aeson,
                        bytestring,
                        base64-bytestring,
                        containers,
                        optparse-applicative,
                        wreq,
                        unordered-containers,
                        hashable,
                        ansi-terminal,
                        time
  default-extensions:   NoMonomorphismRestriction,
                        MagicHash,
                        TemplateHaskell,
                        OverloadedStrings,
                        BangPatterns,
                        FlexibleContexts,
                        FlexibleInstances,
                        UndecidableInstances,
                        Arrows,
                        LambdaCase,
                        MultiWayIf,
                        MultiParamTypeClasses,
                        FunctionalDependencies,
                        DeriveGeneric,
                        RankNTypes,
                        ExplicitForAll,
                        TypeOperators,
                        DefaultSignatures,
                        DataKinds,
                        KindSignatures,
                        ScopedTypeVariables,
                        AllowAmbiguousTypes,
                        InstanceSigs,
                        TypeFamilies,
                        CPP,
                        GeneralizedNewtypeDeriving,
                        StandaloneDeriving,
                        UnicodeSyntax
  ghc-options:          -j8 -ferror-spans -Wall -fno-warn-orphans -fno-warn-name-shadowing -threaded -rtsopts -with-rtsopts=-N8 -with-rtsopts=-qg -O2 $DIR/link-stage/libshm.so
  default-language:     Haskell2010
end
