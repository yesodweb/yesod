#!/bin/sh

CABAL=cabal

# install testing dependencies
$CABAL install HUnit QuickCheck 'hspec >= 0.6.1 && < 0.7' shelltestrunner || exit 1

PACKAGES="yesod-core yesod-json yesod-static yesod-persistent yesod-newsfeed yesod-form yesod-auth yesod-sitemap yesod"
for package in $PACKAGES
do
    echo Installing $package
    cd $package
    ($CABAL configure --enable-tests ||
        ($CABAL install --only-dependencies && $CABAL configure --enable-tests)
    ) && $CABAL build && $CABAL test && ./Setup.lhs install || exit 1
    cd ..
done
