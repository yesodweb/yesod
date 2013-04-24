#!/bin/bash -e

#cabal update

if ! which cabal-nirvana-generate &>/dev/null
then
    cabal install cabal-nirvana -fgenerate
fi

cabal-nirvana-generate yesod yesod-static yesod-default yesod-bin hjsmin blaze-html yesod-test | runghc to-cabal.hs > yesod-platform.cabal
