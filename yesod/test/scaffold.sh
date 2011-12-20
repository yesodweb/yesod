#!/bin/bash -ex

runghc main.hs init

(
  cd foobar
  cabal configure || cabal install
  cabal build
  cabal clean
  cabal configure -fdev
  cabal build
  cabal clean
  cabal configure -flibrary-only
  cabal build
  cabal clean
  cabal configure
  cabal build
)
