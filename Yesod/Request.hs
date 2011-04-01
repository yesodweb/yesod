---------------------------------------------------------
--
-- Module        : Yesod.Request
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- | Provides a parsed version of the raw 'W.Request' data.
--
---------------------------------------------------------
module Yesod.Request
    (
      -- * Request datatype
      RequestBodyContents
    , Request (..)
    , FileInfo (..)
      -- * Convenience functions
    , languages
      -- * Lookup parameters
    , lookupGetParam
    , lookupPostParam
    , lookupCookie
    , lookupFile
      -- ** Multi-lookup
    , lookupGetParams
    , lookupPostParams
    , lookupCookies
    , lookupFiles
    ) where

import Yesod.Internal.Request
import Yesod.Handler
import Control.Monad (liftM)
import Control.Monad.Instances () -- I'm missing the instance Monad ((->) r
import Data.Maybe (listToMaybe)
import Data.Text (Text)

-- | Get the list of supported languages supplied by the user.
--
-- Languages are determined based on the following three (in descending order
-- of preference):
--
-- * The _LANG get parameter.
--
-- * The _LANG cookie.
--
-- * The _LANG user session variable.
--
-- * Accept-Language HTTP header.
--
-- This is handled by parseWaiRequest (not exposed).
languages :: Monad mo => GGHandler s m mo [Text]
languages = reqLangs `liftM` getRequest

lookup' :: Eq a => a -> [(a, b)] -> [b]
lookup' a = map snd . filter (\x -> a == fst x)

-- | Lookup for GET parameters.
lookupGetParams :: Monad mo => Text -> GGHandler s m mo [Text]
lookupGetParams pn = do
    rr <- getRequest
    return $ lookup' pn $ reqGetParams rr

-- | Lookup for GET parameters.
lookupGetParam :: Monad mo => Text -> GGHandler s m mo (Maybe Text)
lookupGetParam = liftM listToMaybe . lookupGetParams

-- | Lookup for POST parameters.
lookupPostParams :: Text -> GHandler s m [Text]
lookupPostParams pn = do
    (pp, _) <- runRequestBody
    return $ lookup' pn pp

lookupPostParam :: Text
                -> GHandler s m (Maybe Text)
lookupPostParam = liftM listToMaybe . lookupPostParams

-- | Lookup for POSTed files.
lookupFile :: Text
           -> GHandler s m (Maybe FileInfo)
lookupFile = liftM listToMaybe . lookupFiles

-- | Lookup for POSTed files.
lookupFiles :: Text
            -> GHandler s m [FileInfo]
lookupFiles pn = do
    (_, files) <- runRequestBody
    return $ lookup' pn files

-- | Lookup for cookie data.
lookupCookie :: Monad mo => Text -> GGHandler s m mo (Maybe Text)
lookupCookie = liftM listToMaybe . lookupCookies

-- | Lookup for cookie data.
lookupCookies :: Monad mo => Text -> GGHandler s m mo [Text]
lookupCookies pn = do
    rr <- getRequest
    return $ lookup' pn $ reqCookies rr
