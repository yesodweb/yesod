#!/bin/bash -e

#cabal update

if ! which cabal-nirvana-generate &>/dev/null
then
    cabal install cabal-nirvana -fgenerate
fi

cabal-nirvana-generate yesod | runghc to-cabal.hs > yesod-platform.cabal
