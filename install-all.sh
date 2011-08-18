#!/bin/sh

CABAL=cabal

# install testing dependencies
$CABAL install HUnit QuickCheck hspec

PACKAGES="yesod-core yesod-json yesod-static yesod-persistent yesod-newsfeed yesod-form yesod-auth yesod-sitemap yesod"
for package in $PACKAGES
do
    echo Installing $package
    cd $package
    $CABAL configure --enable-tests && $CABAL build && $CABAL test && ./Setup.lhs install || exit 1
    cd ..
done
