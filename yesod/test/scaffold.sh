#!/bin/bash -ex

runghc main.hs init

(
  cd foobar
  cabal configure || cabal install
  cabal build
  cabal clean
  cabal configure -fdevel
  cabal build
  cabal clean
  cabal configure -fproduction
  cabal build
)
