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
import Control.Monad.IO.Class
import Control.Monad (liftM)
import Control.Monad.Instances () -- I'm missing the instance Monad ((->) r
import Data.Maybe (listToMaybe)
import qualified Network.HTTP.Types as A

type ParamName = String
type ParamValue = String
type ParamError = String

-- FIXME perhaps remove RequestReader typeclass, include Request datatype in Handler

-- | The reader monad specialized for 'Request'.
class Monad m => RequestReader m where
    getRequest :: m Request
    runRequestBody :: m RequestBodyContents

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
    , reqCookies :: [(A.Ascii, A.Ascii)]
    , reqWaiRequest :: W.Request
      -- | Languages which the client supports.
    , reqLangs :: [String]
      -- | A random, session-specific nonce used to prevent CSRF attacks.
    , reqNonce :: Maybe String
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
lookupPostParams :: RequestReader m
                 => ParamName
                 -> m [ParamValue]
lookupPostParams pn = do
    (pp, _) <- runRequestBody
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
lookupFiles :: RequestReader m
            => ParamName
            -> m [FileInfo]
lookupFiles pn = do
    (_, files) <- runRequestBody
    return $ lookup' pn files

-- | Lookup for cookie data.
lookupCookie :: RequestReader m => A.Ascii -> m (Maybe A.Ascii)
lookupCookie = liftM listToMaybe . lookupCookies

-- | Lookup for cookie data.
lookupCookies :: RequestReader m => A.Ascii -> m [A.Ascii]
lookupCookies pn = do
    rr <- getRequest
    return $ lookup' pn $ reqCookies rr
