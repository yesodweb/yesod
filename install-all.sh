#!/bin/sh

PACKAGES="yesod-core yesod-json yesod-static yesod-persistent yesod-newsfeed yesod-form yesod-auth yesod-sitemap yesod yesod-examples"
CABAL=cabal

for package in $PACKAGES
do
    echo Installing $package
    cd $package
    ($CABAL configure --enable-tests ||
      ($CABAL install --only-dependencies && $CABAL configure --enable-tests)
    ) && $CABAL build && $CABAL test && ./Setup.lhs install || exit
    cd ..
done
