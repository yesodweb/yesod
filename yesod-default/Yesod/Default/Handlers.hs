{-# LANGUAGE OverloadedStrings #-}
module Yesod.Default.Handlers
    ( getFaviconR
    , getRobotsR
    ) where

import Yesod.Handler (GHandler, sendFile)
import Yesod.Content (RepPlain(..), ToContent(..))

getFaviconR :: GHandler s m ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: GHandler s m RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: String)
