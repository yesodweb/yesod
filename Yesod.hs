{-# LANGUAGE CPP #-}
-- | This module simply re-exports from other modules for your convenience.
module Yesod
    ( -- * Re-exports from yesod-core
      module Yesod.Request
    , module Yesod.Content
    , module Yesod.Core
    , module Yesod.Handler
    , module Yesod.Dispatch
    , module Yesod.Widget
      -- * Running your application
    , warp
    , warpDebug
      -- * Commonly referenced functions/datatypes
    , Application
    , lift
    , liftIO
    , MonadPeelIO
      -- * Utilities
    , showIntegral
    , readIntegral
      -- * Hamlet library
      -- ** Hamlet
    , hamlet
    , xhamlet
    , Hamlet
    , Html
    , renderHamlet
    , renderHtml
    , string
    , preEscapedString
    , cdata
      -- ** Julius
    , julius
    , Julius
    , renderJulius
      -- ** Cassius
    , cassius
    , Cassius
    , renderCassius
    ) where

import Yesod.Content
import Yesod.Dispatch
import Yesod.Core
import Yesod.Handler hiding (runHandler)
import Text.Hamlet
import Text.Cassius
import Text.Julius

import Yesod.Request
import Yesod.Widget
import Network.Wai (Application)
import qualified Network.Wai as W
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Peel (MonadPeelIO)

import Network.Wai.Handler.Warp (run)
import System.IO (stderr, hPutStrLn)

showIntegral :: Integral a => a -> String
showIntegral x = show (fromIntegral x :: Integer)

readIntegral :: Num a => String -> Maybe a
readIntegral s =
    case reads s of
        (i, _):_ -> Just $ fromInteger i
        [] -> Nothing

-- | A convenience method to run an application using the Warp webserver on the
-- specified port. Automatically calls 'toWaiApp'.
warp :: (Yesod a, YesodSite a) => Int -> a -> IO ()
warp port a = toWaiApp a >>= run port

-- | Same as 'warp', but also sends a message to stderr for each request, and
-- an \"application launched\" message as well. Can be useful for development.
warpDebug :: (Yesod a, YesodSite a) => Int -> a -> IO ()
warpDebug port a = do
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    toWaiApp a >>= run port . debugMiddleware
  where
    debugMiddleware app req = do
        hPutStrLn stderr $ concat
            [ show $ W.requestMethod req
            , " "
            , show $ W.pathInfo req
            ]
        app req
