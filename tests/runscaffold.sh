#!/bin/sh

cd .. &&
cabal clean && cabal install &&
rm -rf foobar && runghc scaffold.hs init < sample-input.txt && cd foobar && cabal install && cabal install -fdevel && cd .. &&
cd tests
