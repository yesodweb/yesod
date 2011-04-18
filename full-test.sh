#!/bin/sh

cabal clean && cabal install && rm -rf foobar && \
    runghc scaffold.hs init < input-sqlite && cd foobar && cabal install && cabal install -fdevel && cd .. && rm -rf foobar && \
    runghc scaffold.hs init < input-postgres && cd foobar && cabal install && cabal install -fdevel && cd .. && rm -rf foobar && \
    runghc scaffold.hs init < input-mini && cd foobar && cabal install && cabal install -fdevel && cd .. && rm -rf foobar
