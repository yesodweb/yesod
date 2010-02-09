{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
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
    ) where

import Network.HTTP.Wget
import Text.HTML.TagSoup
import Numeric (showHex)
import Control.Monad.Trans
import qualified Safe.Failure as A
import Data.Generics
import Control.Failure hiding (Error)
import Control.Exception

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
getForwardUrl :: (MonadIO m, MonadFailure WgetException m)
              => String -- ^ The openid the user provided.
              -> String -- ^ The URL for this application\'s complete page.
              -> m String -- ^ URL to send the user to.
getForwardUrl openid complete = do
    bodyIdent <- wget openid [] []
    server <- getOpenIdVar "server" bodyIdent
    let delegate = maybe openid id
                 $ getOpenIdVar "delegate" bodyIdent
    return $ constructUrl server
                        [ ("openid.mode", "checkid_setup")
                        , ("openid.identity", delegate)
                        , ("openid.return_to", complete)
                        ]

getOpenIdVar :: Monad m => String -> String -> m String
getOpenIdVar var content = do
    let tags = parseTags content
    let secs = sections (~== ("<link rel=openid." ++ var ++ ">")) tags
    secs' <- mhead secs
    secs'' <- mhead secs'
    return $ fromAttrib "href" secs''
    where
        mhead [] = fail $ "Variable not found: openid." ++ var -- FIXME
        mhead (x:_) = return x

constructUrl :: String -> [(String, String)] -> String
constructUrl url [] = url
constructUrl url args = url ++ "?" ++ queryString args
    where
        queryString [] = error "queryString with empty args cannot happen"
        queryString [first] = onePair first
        queryString (first:rest) = onePair first ++ "&" ++ queryString rest
        onePair (x, y) = urlEncode x ++ "=" ++ urlEncode y

-- | Handle a redirect from an OpenID provider and check that the user
-- logged in properly. If it was successfully, 'return's the openid.
-- Otherwise, 'failure's an explanation.
authenticate :: (MonadIO m, MonadFailure WgetException m,
                 MonadFailure (A.LookupFailure String) m,
                 MonadFailure AuthenticateException m)
             => [(String, String)]
             -> m Identifier
authenticate req = do -- FIXME check openid.mode == id_res (not cancel)
    authUrl <- getAuthUrl req
    content <- wget authUrl [] []
    let isValid = contains "is_valid:true" content
    if isValid
        then A.lookup "openid.identity" req >>= return . Identifier
        else failure $ AuthenticateException content

newtype AuthenticateException = AuthenticateException String
    deriving (Show, Typeable)
instance Exception AuthenticateException

getAuthUrl :: (MonadIO m, MonadFailure (A.LookupFailure String) m,
               MonadFailure WgetException m)
           => [(String, String)] -> m String
getAuthUrl req = do
    identity <- A.lookup "openid.identity" req
    idContent <- wget identity [] []
    helper idContent
    where
        helper :: MonadFailure (A.LookupFailure String) m
               => String -> m String
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
        makeArg :: MonadFailure (A.LookupFailure String) m
                => String -> m (String, String)
        makeArg s = do
            let k = "openid." ++ s
            v <- A.lookup k req
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

urlEncode :: String -> String
urlEncode = concatMap urlEncodeChar

urlEncodeChar :: Char -> String
urlEncodeChar x
    | safeChar (fromEnum x) = return x
    | otherwise = '%' : showHex (fromEnum x) ""

safeChar :: Int -> Bool
safeChar x
    | x >= fromEnum 'a' && x <= fromEnum 'z' = True
    | x >= fromEnum 'A' && x <= fromEnum 'Z' = True
    | x >= fromEnum '0' && x <= fromEnum '9' = True
    | otherwise = False
