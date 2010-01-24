{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
-- Code for extracting parameters from requests.
--
---------------------------------------------------------
module Yesod.Request
    (
      -- * RawRequest
      RawRequest (..)
      -- * Parameter type class
      -- * MonadRequestReader type class and helpers
    , RequestReader (..)
    , getParam
    , postParam
    , anyParam
    , cookieParam
    , identifier
    , displayName
    , acceptedLanguages
    , requestPath
    , parseEnv
    , runRequest
      -- * Building actual request
    , Request (..)
    , Hack.RequestMethod (..)
      -- * Parameter restrictions
    -- FIXME , notBlank
#if TEST
    , testSuite
#endif
    ) where

import qualified Hack
import Data.Function.Predicate (equals)
import Yesod.Constants
import Yesod.Utils (tryLookup)
import Yesod.Definitions
import Yesod.Parameter
import Control.Applicative (Applicative (..))
import Web.Encodings
import qualified Data.ByteString.Lazy as BL
import Data.Convertible.Text
import Hack.Middleware.CleanPath (splitPath)
import Control.Arrow ((***))
import Control.Exception (Exception, SomeException (..))
import Data.Typeable (Typeable)
import Data.Attempt

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
#endif

newtype Request v = Request { unRequest :: RawRequest
                                        -> Either ParamException v }
instance Functor Request where
    fmap f (Request r) = Request $ fmap f . r
instance Applicative Request where
    pure = Request . const . Right
    (Request f) <*> (Request r) = Request helper where
        helper rr = helper2 (f rr) (r rr)
        helper2 (Left e1) (Left e2) = Left $ e1 ++ e2
        helper2 (Left e) _ = Left e
        helper2 _ (Left e) = Left e
        helper2 (Right f') (Right r') = Right $ f' r'

class RequestReader m where
    getRawRequest :: m RawRequest
    invalidParams :: ParamException -> m a
instance RequestReader Request where
    getRawRequest = Request $ Right
    invalidParams = Request . const . Left

runRequest :: (Monad m, RequestReader m) => Request a -> m a
runRequest (Request f) = do
    rr <- getRawRequest
    either invalidParams return $ f rr
{- FIXME
-- | Attempt to parse a list of param values using 'readParams'.
-- If that fails, return an error message and an undefined value. This way,
-- we can process all of the parameters and get all of the error messages.
-- Be careful not to use the value inside until you can be certain the
-- reading succeeded.
tryReadParams:: (Parameter a, MonadRequestReader m)
             => ParamType
             -> ParamName
             -> [RawParam]
             -> m a
tryReadParams ptype name params =
    case readParams params of
        Failure s -> invalidParam ptype name s
        Success x -> return x
-}

-- | Helper function for generating 'RequestParser's from various
-- 'ParamValue' lists.
genParam :: Parameter a
         => (RawRequest -> ParamName -> [ParamValue])
         -> ParamType
         -> ParamName
         -> Request a
genParam f ptype name = Request helper where
  helper req = attempt failureH Right $ readParams pvs where
      pvs = f req name
      failureH e = Left [((ptype, name, pvs), SomeException e)]

-- | Parse a value passed as a GET parameter.
getParam :: (Parameter a) => ParamName -> Request a
getParam = genParam getParams GetParam

-- | Parse a value passed as a POST parameter.
postParam :: (Parameter a) => ParamName -> Request a
postParam = genParam postParams PostParam

-- | Parse a value passed as a GET, POST or URL parameter.
anyParam :: (Parameter a) => ParamName -> Request a
anyParam = genParam anyParams PostParam -- FIXME

-- | Parse a value passed as a raw cookie.
cookieParam :: (Parameter a) => ParamName -> Request a
cookieParam = genParam cookies CookieParam

-- | Extract the cookie which specifies the identifier for a logged in
-- user, if available.
identifier :: (Functor m, Monad m, RequestReader m) => m (Maybe String)
identifier = do
    env <- parseEnv
    case lookup authCookieName $ Hack.hackHeaders env of
        Nothing -> return Nothing
        Just x -> return (Just x)

displayName :: (Functor m, Monad m, RequestReader m) => m (Maybe String)
displayName = do
    env <- parseEnv
    case lookup authDisplayName $ Hack.hackHeaders env of
        Nothing -> return Nothing
        Just x -> return (Just x)

-- | Get the raw 'Hack.Env' value.
parseEnv :: (Functor m, RequestReader m) => m Hack.Env
parseEnv = rawEnv `fmap` getRawRequest

-- | Determine the ordered list of language preferences.
--
-- FIXME: Future versions should account for some cookie.
acceptedLanguages :: (Functor m, Monad m, RequestReader m) => m [String]
acceptedLanguages = do
    env <- parseEnv
    let rawLang = tryLookup "" "Accept-Language" $ Hack.http env
    return $! parseHttpAccept rawLang

-- | Determinge the path requested by the user (ie, the path info).
requestPath :: (Functor m, Monad m, RequestReader m) => m String
requestPath = do
    env <- parseEnv
    let q = case Hack.queryString env of
                "" -> ""
                q'@('?':_) -> q'
                q' -> q'
    return $! dropSlash (Hack.pathInfo env) ++ q
      where
        dropSlash ('/':x) = x
        dropSlash x = x

-- | The raw information passed through Hack, cleaned up a bit.
data RawRequest = RawRequest
    { rawPathInfo :: PathInfo
    , rawGetParams :: [(ParamName, ParamValue)]
    , rawPostParams :: [(ParamName, ParamValue)]
    , rawCookies :: [(ParamName, ParamValue)]
    , rawFiles :: [(ParamName, FileInfo String BL.ByteString)]
    , rawEnv :: Hack.Env
    , rawLanguages :: [Language]
    }
    deriving Show

-- | All GET paramater values with the given name.
getParams :: RawRequest -> ParamName -> [ParamValue]
getParams rr name = map snd
                  . filter (\x -> name == fst x)
                  . rawGetParams
                  $ rr

-- | All POST paramater values with the given name.
postParams :: RawRequest -> ParamName -> [ParamValue]
postParams rr name = map snd
                   . filter (\x -> name == fst x)
                   . rawPostParams
                   $ rr

-- | All GET and POST paramater values (see rewriting) with the given name.
anyParams :: RawRequest -> ParamName -> [ParamValue]
anyParams req name = getParams req name ++
                     postParams req name

-- | All cookies with the given name.
cookies :: RawRequest -> ParamName -> [ParamValue]
cookies rr name = map snd . filter (fst `equals` name) . rawCookies $ rr

{- FIXME
-- | Ensures that a String parameter is not blank.
notBlank :: MonadRequestReader m => RawParam -> m String
notBlank rp =
  case paramValue rp of
    "" -> invalidParam (paramType rp) (paramName rp) RequiredField
    s -> return s
-}

data RequiredField = RequiredField
    deriving (Show, Typeable)
instance Exception RequiredField

instance ConvertSuccess Hack.Env RawRequest where
  convertSuccess env =
    let (Right rawPieces) = splitPath $ Hack.pathInfo env
        gets' = decodeUrlPairs $ Hack.queryString env :: [(String, String)]
        clength = tryLookup "0" "Content-Length" $ Hack.http env
        ctype = tryLookup "" "Content-Type" $ Hack.http env
        convertFileInfo (FileInfo a b c) = FileInfo (cs a) (cs b) c
        (posts, files) = map (convertSuccess *** convertSuccess) ***
                         map (convertSuccess *** convertFileInfo)
                       $ parsePost ctype clength
                       $ Hack.hackInput env
        rawCookie = tryLookup "" "Cookie" $ Hack.http env
        cookies' = decodeCookies rawCookie :: [(String, String)]
        langs = ["en"] -- FIXME
     in RawRequest rawPieces gets' posts cookies' files env langs

#if TEST
testSuite :: Test
testSuite = testGroup "Yesod.Request"
    [ testCase "Request applicative instance" caseAppInst
    ]

caseAppInst :: Assertion
caseAppInst = do
    let r5 = Request $ const $ Right 5
        rAdd2 = Request $ const $ Right (+ 2)
        r7 = Request $ const $ Right 7
        rr = undefined
        myEquals e t = (unRequest e) rr `myEquals2` (unRequest t) rr
        myEquals2 x y = show x @=? show y
    r5 `myEquals` pure 5
    r7 `myEquals` (rAdd2 <*> r5)
#endif
