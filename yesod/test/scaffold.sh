#!/bin/bash -x

rm -rf foobar && runghc main.hs init && cd foobar && cabal install && cabal install -fdevel && cd ..
