#!/bin/bash -ex

rm -rf foobar
runghc main.hs init

(
  cd foobar
  cabal install
  cabal install -fdevel
)

ghc-pkg unregister foobar
