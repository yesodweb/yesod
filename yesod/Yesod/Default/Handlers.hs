{-# LANGUAGE OverloadedStrings #-}
module Yesod.Default.Handlers
    ( getFaviconR
    , getRobotsR
    ) where

import Yesod.Core

getFaviconR :: HandlerError m => m ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: HandlerError m => m ()
getRobotsR = sendFile "text/plain" "config/robots.txt"
