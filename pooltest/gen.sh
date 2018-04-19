#!/usr/bin/env bash

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

cat > pooltest/stack.yaml <<EOF
flags:
  time-locale-compat:
    old-locale: false
extra-package-dbs: []
packages:
- '.'
extra-deps:
- RSA-2.2.0
- SHA-1.6.4.2
- StateVar-1.1.0.3
- adjunctions-4.3
- aeson-0.11.0.0
- ansi-terminal-0.6.2.3
- ansi-wl-pprint-0.6.7.3
- asn1-encoding-0.9.3
- asn1-parse-0.9.4
- asn1-types-0.3.2
- async-2.1.0
- attoparsec-0.13.0.1
- authenticate-oauth-1.5.1.1
- base-orphans-0.5.1
- base16-bytestring-0.1.1.6
- base64-bytestring-1.0.0.1
- bifunctors-5.2.1
- blaze-builder-0.4.0.1
- byteable-0.1.1
- case-insensitive-1.2.0.5
- cereal-0.5.1.0
- comonad-5
- connection-0.2.5
- contravariant-1.4
- cookie-0.4.1.6
- crypto-api-0.13.2
- crypto-pubkey-types-0.4.3
- cryptohash-0.11.6
- cryptonite-0.11
- data-default-0.5.3
- data-default-class-0.0.1
- data-default-instances-base-0.0.1
- data-default-instances-containers-0.0.1
- data-default-instances-dlist-0.0.1
- data-default-instances-old-locale-0.0.1
- distributive-0.5.0.2
- dlist-0.7.1.2
- entropy-0.3.7
- exceptions-0.8.2.1
- fail-4.9.0.0
- free-4.12.4
- hashable-1.2.4.0
- hourglass-0.2.9
- http-client-0.4.27
- http-client-tls-0.2.2
- http-types-0.9
- kan-extensions-5.0.1
- lens-4.13.2
- lens-aeson-1.0.0.5
- memory-0.11
- mime-types-0.1.0.6
- mtl-2.2.1
- network-2.6.2.1
- network-uri-2.6.0.3
- old-locale-1.0.0.7
- optparse-applicative-0.12.1.0
- parallel-3.2.1.0
- parsec-3.1.9
- pem-0.2.2
- prelude-extras-0.4.0.3
- primitive-0.6.1.0
- profunctors-5.2
- psqueues-0.2.2.0
- pureMD5-2.1.2.1
- random-1.1
- reflection-2.1.2
- scientific-0.3.4.4
- semigroupoids-5.0.1
- semigroups-0.18.1
- socks-0.5.4
- stm-2.4.4.1
- streaming-commons-0.1.15.1
- syb-0.6
- tagged-0.8.3
- text-1.2.2.0
- time-locale-compat-0.1.1.1
- tls-1.3.4
- transformers-compat-0.5.1.4
- unordered-containers-0.2.6.0
- vector-0.11.0.0
- void-0.7.1
- wreq-0.4.1.0
- x509-1.6.3
- x509-store-1.6.1
- x509-system-1.6.3
- x509-validation-1.6.3
- zlib-0.6.1.1
resolver: ghc-7.10.3
compiler: ghc-7.10.3
pkgconfig-depends: zlib
EOF

if [[ -n "$NIXOS" ]]; then
	cat >> pooltest/stack.yaml <<EOF
nix:
    enable: true
    pure: false
    add-gc-roots: true
    packages: [zlib.dev, zlib.out, pkgconfig]
EOF
fi
