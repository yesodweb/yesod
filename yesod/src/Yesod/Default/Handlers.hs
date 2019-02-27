{-# LANGUAGE OverloadedStrings #-}
module Yesod.Default.Handlers
    ( getFaviconR
    , getRobotsR
    ) where

import Yesod.Core

getFaviconR :: HasHandlerData env => RIO env ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: HasHandlerData env => RIO env ()
getRobotsR = sendFile "text/plain" "config/robots.txt"
