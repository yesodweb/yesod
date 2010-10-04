{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
---------------------------------------------------------
-- |
-- Module        : Web.Authenticate.OpenId
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Provides functionality for being an OpenId consumer.
--
---------------------------------------------------------
module Web.Authenticate.OpenId
    ( Identifier (..)
    , getForwardUrl
    , authenticate
    , AuthenticateException (..)
    ) where

import Network.HTTP.Enumerator
import Text.HTML.TagSoup
import "transformers" Control.Monad.IO.Class
import Data.Data
import Control.Failure hiding (Error)
import Control.Exception
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy.Char8 as L8
import Web.Authenticate.Internal (qsEncode)
import Data.List (intercalate)

-- | An openid identifier (ie, a URL).
newtype Identifier = Identifier { identifier :: String }
    deriving (Eq, Show)

data Error v = Error String | Ok v
instance Monad Error where
    return = Ok
    Error s >>= _ = Error s
    Ok v >>= f = f v
    fail s = Error s

-- | Returns a URL to forward the user to in order to login.
getForwardUrl :: (MonadIO m,
                  Failure InvalidUrlException m,
                  Failure HttpException m,
                  Failure MissingVar m
                  )
              => String -- ^ The openid the user provided.
              -> String -- ^ The URL for this application\'s complete page.
              -> m String -- ^ URL to send the user to.
getForwardUrl openid complete = do
    bodyIdent' <- simpleHttp openid
    let bodyIdent = L8.unpack bodyIdent'
    server <- getOpenIdVar "server" bodyIdent
    let delegate = maybe openid id
                 $ getOpenIdVar "delegate" bodyIdent
    return $ constructUrl server
                        [ ("openid.mode", "checkid_setup")
                        , ("openid.identity", delegate)
                        , ("openid.return_to", complete)
                        ]

data MissingVar = MissingVar String
    deriving (Typeable, Show)
instance Exception MissingVar

getOpenIdVar :: Failure MissingVar m => String -> String -> m String
getOpenIdVar var content = do
    let tags = parseTags content
    let secs = sections (~== ("<link rel=openid." ++ var ++ ">")) tags
    secs' <- mhead secs
    secs'' <- mhead secs'
    return $ fromAttrib "href" secs''
    where
        mhead [] = failure $ MissingVar $ "openid." ++ var
        mhead (x:_) = return x

constructUrl :: String -> [(String, String)] -> String
constructUrl url [] = url
constructUrl url args =
    url ++ "?" ++ intercalate "&" (map qsPair args)
  where
    qsPair (x, y) = qsEncode x ++ '=' : qsEncode y 

-- | Handle a redirect from an OpenID provider and check that the user
-- logged in properly. If it was successfully, 'return's the openid.
-- Otherwise, 'failure's an explanation.
authenticate :: (MonadIO m,
                 Failure AuthenticateException m,
                 Failure InvalidUrlException m,
                 Failure HttpException m,
                 Failure MissingVar m)
             => [(String, String)]
             -> m Identifier
authenticate req = do -- FIXME check openid.mode == id_res (not cancel)
    authUrl <- getAuthUrl req
    content' <- simpleHttp authUrl
    let content = L8.unpack content'
    let isValid = contains "is_valid:true" content
    if isValid
        then Identifier `liftM` alookup "openid.identity" req
        else failure $ AuthenticateException content

alookup :: (Failure AuthenticateException m, Monad m)
        => String
        -> [(String, String)]
        -> m String
alookup k x = case lookup k x of
                Just k' -> return k'
                Nothing -> failure $ MissingOpenIdParameter k

data AuthenticateException = AuthenticateException String
                           | MissingOpenIdParameter String
    deriving (Show, Typeable)
instance Exception AuthenticateException

getAuthUrl :: (MonadIO m, Failure AuthenticateException m,
               Failure InvalidUrlException m,
               Failure HttpException m,
               Failure MissingVar m)
           => [(String, String)] -> m String
getAuthUrl req = do
    identity <- alookup "openid.identity" req
    idContent <- simpleHttp identity
    helper $ L8.unpack idContent
    where
        helper idContent = do
            server <- getOpenIdVar "server" idContent
            dargs <- mapM makeArg [
                "assoc_handle",
                "sig",
                "signed",
                "identity",
                "return_to"
                ]
            let sargs = [("openid.mode", "check_authentication")]
            return $ constructUrl server $ dargs ++ sargs
        makeArg s = do
            let k = "openid." ++ s
            v <- alookup k req
            return (k, v)

contains :: String -> String -> Bool
contains [] _ = True
contains _ [] = False
contains needle haystack =
    begins needle haystack ||
    (contains needle $ tail haystack)

begins :: String -> String -> Bool
begins [] _ = True
begins _ [] = False
begins (x:xs) (y:ys) = x == y && begins xs ys
