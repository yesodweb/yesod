#!/bin/bash -x

rm -rf foobar && runghc scaffold.hs init && cd foobar && cabal install && cabal install -fdevel && cd ..
