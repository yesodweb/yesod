#!/bin/sh

cabal clean && cabal install && rm -rf foobar && \
    runghc scaffold.hs < input-sqlite && cd foobar && cabal install && cd .. && \
    runghc scaffold.hs < input-postgres && cd foobar && cabal install && cd .. && \
    runghc scaffold.hs < input-mini && cd foobar && cabal install && cd .. && \
    rm -rf foobar
