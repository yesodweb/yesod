#!/bin/bash -ex

runghc -i. -idist/build/autogen main.hs init

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
  cabal configure --enable-tests
  cabal build
  cabal test
)
