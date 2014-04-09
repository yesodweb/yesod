#!/bin/bash -e

#cabal update

if ! which cabal-nirvana-generate &>/dev/null
then
    cabal install cabal-nirvana -fgenerate
fi

cabal-nirvana-generate \
    yesod \
    yesod-static \
    hjsmin \
    blaze-html \
    yesod-test \
    shakespeare-text \
    esqueleto \
    warp-tls \
    hjsmin \
    http-reverse-proxy \
    | runghc to-cabal.hs > yesod-platform.cabal
