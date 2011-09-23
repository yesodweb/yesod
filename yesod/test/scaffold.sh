#!/bin/bash -ex

runghc main.hs init

(
  cd foobar
  cabal install
  cabal install -fdevel
  cabal install -fproduction
)
