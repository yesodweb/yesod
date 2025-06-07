{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod.Dispatch
import Yesod.Content
import Yesod.Internal.Core
import Data.ByteString (ByteString)
import Network.Wai.Handler.Warp (run)
import Control.Concurrent.MVar
import Control.Concurrent
import Network.Wai
import Control.Monad.IO.Class

data Pong = Pong
mkYesod "Pong" [$parseRoutes|
/ PongR GET
|]

instance Yesod Pong where
    approot _ = ""
    encryptKey _ = return Nothing

getPongR = return $ RepPlain $ toContent ("PONG" :: ByteString)

main = do
    app <- toWaiAppPlain Pong
    flag <- newEmptyMVar
    forkIO $ run 3000 $ \req ->
        if pathInfo req == ["kill"]
            then do
                liftIO $ putMVar flag ()
                error "done"
            else app req
    takeMVar flag
