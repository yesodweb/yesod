{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod.Core
import Network.Wai.Handler.Warp

data Pong = Pong

mkYesod "Pong" [parseRoutes|
/ HomeR GET
|]

instance Yesod Pong

getHomeR = liftHandlerT $ return "PONG"

main = warp 3000 Pong
