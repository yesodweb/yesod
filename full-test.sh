#!/bin/sh

cabal clean && cabal install && rm -rf foobar && \
    yesod < input-sqlite && cd foobar && cabal install && cd .. && \
    yesod < input-postgres && cd foobar && cabal install && cd .. && \
    yesod < input-mini && cd foobar && cabal install && cd .. && \
    rm -rf foobar
