#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/.. && pwd )"

cat <<end > aslam/aslam.cabal
name:           aslam
version:        0.1.0.0
synopsis:       Adaptive SLAM
license:        AllRightsReserved
author:         Christopher Goes
maintainer:     cwgoes@protonmail.ch
copyright:      2016 Christopher Goes
build-type:     Simple
cabal-version:  >=1.10

executable aslam
  hs-source-dirs:       src
                        ../libshm/haskell
                        ../auvlog
  main-is:              Core/CLI.hs
  build-depends:        base,
                        text,
                        lzma,
                        zlib,
                        data-default,
                        aeson,
                        attoparsec,
                        vector-algorithms,
                        pseudomacros,
                        normaldistribution,
                        binary,
                        bytestring,
                        base64-bytestring,
                        containers,
                        optparse-applicative,
                        unordered-containers,
                        hashable,
                        uuid,
                        vector,
                        template-haskell,
                        safe,
                        deepseq,
                        snap-server,
                        snap-core,
                        random,
                        transformers,
                        nanomsg-haskell,
                        websockets,
                        websockets-snap,
                        double-conversion,
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
