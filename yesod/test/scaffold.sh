#!/bin/bash -ex

runghc main.hs init

(
  cd foobar
  cabal configure --disable-optimization || cabal install
  cabal build
  cabal clean
  cabal configure -fdev --disable-optimization
  cabal build
  cabal clean
  cabal configure -flibrary-only --disable-optimization
  cabal build
  cabal clean
  cabal configure
  cabal build
)
