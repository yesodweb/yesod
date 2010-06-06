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
    , lookupSession
      -- ** Alternate
    , getParams
    , postParams
    , cookies
    , session
      -- * Parameter type synonyms
    , ParamName
    , ParamValue
    , ParamError
    ) where

import qualified Network.Wai as W
import qualified Data.ByteString.Lazy as BL
import "transformers" Control.Monad.IO.Class
import Control.Monad (liftM)
import Network.Wai.Parse
import Control.Monad.Instances () -- I'm missing the instance Monad ((->) r

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
-- of preference:
--
-- * The _LANG get parameter.
--
-- * The _LANG cookie.
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
    , [(ParamName, FileInfo BL.ByteString)]
    )

-- | The parsed request information.
data Request = Request
    { reqGetParams :: [(ParamName, ParamValue)]
    , reqCookies :: [(ParamName, ParamValue)]
      -- | Session data stored in a cookie via the clientsession package.
    , reqSession :: [(ParamName, ParamValue)]
      -- | The POST parameters and submitted files. This is stored in an IO
      -- thunk, which essentially means it will be computed once at most, but
      -- only if requested. This allows avoidance of the potentially costly
      -- parsing of POST bodies for pages which do not use them.
    , reqRequestBody :: IO RequestBodyContents
    , reqWaiRequest :: W.Request
      -- | Languages which the client supports.
    , reqLangs :: [String]
    }

multiLookup :: [(ParamName, ParamValue)] -> ParamName -> [ParamValue]
multiLookup [] _ = []
multiLookup ((k, v):rest) pn
    | k == pn = v : multiLookup rest pn
    | otherwise = multiLookup rest pn

-- | All GET paramater values with the given name.
getParams :: RequestReader m => m (ParamName -> [ParamValue])
getParams = do
    rr <- getRequest
    return $ multiLookup $ reqGetParams rr

-- | Lookup for GET parameters.
lookupGetParam :: RequestReader m => ParamName -> m (Maybe ParamValue)
lookupGetParam pn = do
    rr <- getRequest
    return $ lookup pn $ reqGetParams rr

-- | All POST paramater values with the given name.
postParams :: MonadIO m => Request -> m (ParamName -> [ParamValue])
postParams rr = do
    (pp, _) <- liftIO $ reqRequestBody rr
    return $ multiLookup pp

-- | Lookup for POST parameters.
lookupPostParam :: (MonadIO m, RequestReader m)
                => ParamName
                -> m (Maybe ParamValue)
lookupPostParam pn = do
    rr <- getRequest
    (pp, _) <- liftIO $ reqRequestBody rr
    return $ lookup pn pp

-- | All cookies with the given name.
cookies :: RequestReader m => m (ParamName -> [ParamValue])
cookies = do
    rr <- getRequest
    return $ multiLookup $ reqCookies rr

-- | Lookup for cookie data.
lookupCookie :: RequestReader m => ParamName -> m (Maybe ParamValue)
lookupCookie pn = do
    rr <- getRequest
    return $ lookup pn $ reqCookies rr

-- | All session data with the given name.
session :: RequestReader m => m (ParamName -> [ParamValue])
session = do
    rr <- getRequest
    return $ multiLookup $ reqSession rr

-- | Lookup for session data.
lookupSession :: RequestReader m => ParamName -> m (Maybe ParamValue)
lookupSession pn = do
    rr <- getRequest
    return $ lookup pn $ reqSession rr
