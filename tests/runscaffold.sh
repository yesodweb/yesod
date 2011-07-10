#!/bin/sh

cabal clean && cabal install &&
  rm -rf foobar && runghc scaffold.hs init < tests/sample-input.txt && cd foobar && cabal install && cabal install -fdevel && cd ..
