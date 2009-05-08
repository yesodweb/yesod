{-# LANGUAGE FlexibleInstances #-}
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

import Data.Maybe (fromMaybe, fromJust)
import Network.HTTP.Wget
import Text.HTML.TagSoup
import Numeric (showHex)

-- | An openid identifier (ie, a URL).
data Identifier = Identifier { identifier :: String }

instance Monad (Either String) where
    return = Right
    fail = Left
    (Left s) >>= _ = Left s
    (Right x) >>= f = f x

-- | Returns a URL to forward the user to in order to login.
getForwardUrl :: Monad m
              => String -- ^ The openid the user provided.
              -> String -- ^ The URL for this application\'s complete page.
              -> IO (m String) -- ^ URL to send the user to.
getForwardUrl openid complete = do
    bodyIdent' <- wget openid [] []
    case bodyIdent' of
        Left s -> return $ fail s
        Right bodyIdent -> do
        server <- getOpenIdVar "server" bodyIdent
        let delegate = fromMaybe openid $ getOpenIdVar "delegate" bodyIdent
        return $ return $ constructUrl server
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
        mhead [] = fail $ "Variable not found: openid." ++ var
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
-- Otherwise, 'fail's an explanation.
authenticate :: Monad m => [(String, String)] -> IO (m Identifier)
authenticate req = do -- FIXME check openid.mode == id_res (not cancel)
    authUrl' <- getAuthUrl req
    case authUrl' of
        Nothing -> return $ fail "Invalid parameters"
        Just authUrl -> do
            content' <- wget authUrl [] []
            case content' of
                Left s -> return $ fail s
                Right content -> do
                    let isValid = contains "is_valid:true" content
                    if isValid
                        then return $
                             return $ Identifier
                             (fromJust $ lookup "openid.identity" req)
                        else return $ fail content

getAuthUrl :: [(String, String)] -> IO (Maybe String)
getAuthUrl req = do
    let identity' = lookup "openid.identity" req
    case identity' of
        Nothing -> return Nothing
        Just identity -> do
            idContent <- wget identity [] []
            case idContent of
                Nothing -> return Nothing
                Just x -> return $ helper x
    where
        helper :: String -> Maybe String
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
        makeArg :: String -> Maybe (String, String)
        makeArg s = do
            let k = "openid." ++ s
            v <- lookup k req
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
