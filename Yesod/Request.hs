{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
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
    , RequestReader (..)
    , FileInfo (..)
      -- * Convenience functions
    , waiRequest
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
      -- * Parameter type synonyms
    , ParamName
    , ParamValue
    , ParamError
    ) where

import qualified Network.Wai as W
import qualified Data.ByteString.Lazy as BL
import "transformers" Control.Monad.IO.Class
import Control.Monad (liftM)
import Control.Monad.Instances () -- I'm missing the instance Monad ((->) r
import Data.Maybe (listToMaybe)

type ParamName = String
type ParamValue = String
type ParamError = String

-- | The reader monad specialized for 'Request'.
class Monad m => RequestReader m where
    getRequest :: m Request
instance RequestReader ((->) Request) where
    getRequest = id

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
-- This is handled by the parseWaiRequest function in Yesod.Dispatch (not
-- exposed).
languages :: RequestReader m => m [String]
languages = reqLangs `liftM` getRequest

-- | Get the request\'s 'W.Request' value.
waiRequest :: RequestReader m => m W.Request
waiRequest = reqWaiRequest `liftM` getRequest

-- | A tuple containing both the POST parameters and submitted files.
type RequestBodyContents =
    ( [(ParamName, ParamValue)]
    , [(ParamName, FileInfo)]
    )

data FileInfo = FileInfo
    { fileName :: String
    , fileContentType :: String
    , fileContent :: BL.ByteString
    }
    deriving (Eq, Show)

-- | The parsed request information.
data Request = Request
    { reqGetParams :: [(ParamName, ParamValue)]
    , reqCookies :: [(ParamName, ParamValue)]
      -- | The POST parameters and submitted files. This is stored in an IO
      -- thunk, which essentially means it will be computed once at most, but
      -- only if requested. This allows avoidance of the potentially costly
      -- parsing of POST bodies for pages which do not use them.
    , reqRequestBody :: IO RequestBodyContents
    , reqWaiRequest :: W.Request
      -- | Languages which the client supports.
    , reqLangs :: [String]
      -- | A random, session-specific nonce used to prevent CSRF attacks.
    , reqNonce :: String
    }

lookup' :: Eq a => a -> [(a, b)] -> [b]
lookup' a = map snd . filter (\x -> a == fst x)

-- | Lookup for GET parameters.
lookupGetParams :: RequestReader m => ParamName -> m [ParamValue]
lookupGetParams pn = do
    rr <- getRequest
    return $ lookup' pn $ reqGetParams rr

-- | Lookup for GET parameters.
lookupGetParam :: RequestReader m => ParamName -> m (Maybe ParamValue)
lookupGetParam = liftM listToMaybe . lookupGetParams

-- | Lookup for POST parameters.
lookupPostParams :: (MonadIO m, RequestReader m)
                 => ParamName
                 -> m [ParamValue]
lookupPostParams pn = do
    rr <- getRequest
    (pp, _) <- liftIO $ reqRequestBody rr
    return $ lookup' pn pp

lookupPostParam :: (MonadIO m, RequestReader m)
                => ParamName
                -> m (Maybe ParamValue)
lookupPostParam = liftM listToMaybe . lookupPostParams

-- | Lookup for POSTed files.
lookupFile :: (MonadIO m, RequestReader m)
           => ParamName
           -> m (Maybe FileInfo)
lookupFile = liftM listToMaybe . lookupFiles

-- | Lookup for POSTed files.
lookupFiles :: (MonadIO m, RequestReader m)
            => ParamName
            -> m [FileInfo]
lookupFiles pn = do
    rr <- getRequest
    (_, files) <- liftIO $ reqRequestBody rr
    return $ lookup' pn files

-- | Lookup for cookie data.
lookupCookie :: RequestReader m => ParamName -> m (Maybe ParamValue)
lookupCookie = liftM listToMaybe . lookupCookies

-- | Lookup for cookie data.
lookupCookies :: RequestReader m => ParamName -> m [ParamValue]
lookupCookies pn = do
    rr <- getRequest
    return $ lookup' pn $ reqCookies rr
