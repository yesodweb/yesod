{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Core.Dispatch
    ( -- * Quasi-quoted routing
      parseRoutes
    , parseRoutesNoCheck
    , parseRoutesFile
    , parseRoutesFileNoCheck
    , mkYesod
      -- ** More fine-grained
    , mkYesodData
    , mkYesodSubData
    , mkYesodDispatch
    , mkYesodSubDispatch
      -- ** Path pieces
    , PathPiece (..)
    , PathMultiPiece (..)
    , Texts
      -- * Convert to WAI
    , toWaiApp
      -- * WAI subsites
    , WaiSubsite (..)
    ) where

import Prelude hiding (exp)
import Yesod.Core.Internal.TH

import Web.PathPieces

import qualified Network.Wai as W

import Data.ByteString.Lazy.Char8 ()

import Data.Text (Text)
import Data.Monoid (mappend)
import qualified Data.ByteString as S
import qualified Blaze.ByteString.Builder
import Network.HTTP.Types (status301)
import Yesod.Routes.Parse
import Yesod.Core.Types
import Yesod.Core.Class.Yesod
import Yesod.Core.Class.Dispatch
import Yesod.Core.Internal.Run

-- | Convert the given argument into a WAI application, executable with any WAI
-- handler. Note that, in versions of Yesod prior to 1.2, this would include
-- some default middlewares (GZIP and autohead). This is no longer the case; if
-- you want these middlewares, you should provide them yourself.
toWaiApp :: YesodDispatch site => site -> IO W.Application
toWaiApp site = do
    logger <- makeLogger site
    sb <- makeSessionBackend site
    let yre = YesodRunnerEnv
            { yreLogger = logger
            , yreSite = site
            , yreSessionBackend = sb
            }
    return $ \req ->
        case cleanPath site $ W.pathInfo req of
            Left pieces -> sendRedirect site pieces req
            Right pieces -> yesodDispatch yre req
                { W.pathInfo = pieces
                }
  where
    sendRedirect :: Yesod master => master -> [Text] -> W.Application
    sendRedirect y segments' env =
         return $ W.responseLBS status301
                [ ("Content-Type", "text/plain")
                , ("Location", Blaze.ByteString.Builder.toByteString dest')
                ] "Redirecting"
      where
        dest = joinPath y (resolveApproot y env) segments' []
        dest' =
            if S.null (W.rawQueryString env)
                then dest
                else (dest `mappend`
                     Blaze.ByteString.Builder.fromByteString (W.rawQueryString env))
