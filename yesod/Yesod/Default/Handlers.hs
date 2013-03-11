{-# LANGUAGE OverloadedStrings #-}
module Yesod.Default.Handlers
    ( getFaviconR
    , getRobotsR
    ) where

import Yesod.Handler (GHandler, sendFile)
import Yesod.Content (RepPlain(..))

getFaviconR :: GHandler s m ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: GHandler s m RepPlain
getRobotsR = sendFile "text/plain" "config/robots.txt"
