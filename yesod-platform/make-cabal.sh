#!/bin/bash -e

#cabal update

if ! which cabal-nirvana-generate &>/dev/null
then
    cabal install cabal-nirvana -fgenerate
fi

cabal-nirvana-generate yesod yesod-static yesod-default hjsmin blaze-html | runghc to-cabal.hs > yesod-platform.cabal
