{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Import
import Settings
import Yesod.Logger (defaultDevelopmentLogger)
import Yesod.Default.Config
import Yesod.Test
import Application (makeFoundation)

import HomeTest

main :: IO a
main = do
    conf <- loadConfig $ (configSettings Testing) { csParseExtra = parseExtra }
    logger <- defaultDevelopmentLogger
    foundation <- makeFoundation conf logger
    app <- toWaiAppPlain foundation
    runTests app (connPool foundation) homeSpecs
