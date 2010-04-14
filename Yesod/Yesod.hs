{-# LANGUAGE QuasiQuotes #-}
-- | The basic typeclass for a Yesod application.
module Yesod.Yesod
    ( Yesod (..)
    , YesodSite (..)
    , simpleApplyLayout
    , getApproot
    , toWaiApp
    , basicHandler
    ) where

import Yesod.Response
import Yesod.Request
import Yesod.Definitions
import Yesod.Hamlet
import Yesod.Handler hiding (badMethod)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Convertible.Text
import Text.Hamlet.Monad (fromList)

import Data.Maybe (fromMaybe)
import Web.Mime
import Web.Encodings (parseHttpAccept)
import Web.Routes (Site (..), encodePathInfo, decodePathInfo)
import Data.List (intercalate)

import qualified Network.Wai as W
import Network.Wai.Middleware.CleanPath
import Network.Wai.Middleware.ClientSession
import Network.Wai.Middleware.Jsonp
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Middleware.Gzip

import qualified Network.Wai.Handler.SimpleServer as SS
import qualified Network.Wai.Handler.CGI as CGI
import System.Environment (getEnvironment)

class YesodSite y where
    getSite :: Site (Routes y) (String -> YesodApp -> y -> YesodApp)

class YesodSite a => Yesod a where
    -- | The encryption key to be used for encrypting client sessions.
    encryptKey :: a -> IO Word256
    encryptKey _ = getKey defaultKeyFile

    -- | Number of minutes before a client session times out. Defaults to
    -- 120 (2 hours).
    clientSessionDuration :: a -> Int
    clientSessionDuration = const 120

    -- | Output error response pages.
    errorHandler :: Yesod y => a -> ErrorResponse -> Handler y ChooseRep
    errorHandler _ = defaultErrorHandler

    -- | Applies some form of layout to <title> and <body> contents of a page. FIXME: use a Maybe here to allow subsites to simply inherit.
    applyLayout :: a
                -> PageContent (Routes a)
                -> Request
                -> Hamlet (Routes a) IO ()
    applyLayout _ p _ = [$hamlet|
!!!
%html
    %head
        %title $pageTitle$
        ^pageHead^
    %body
        ^pageBody^
|] p

    -- | Gets called at the beginning of each request. Useful for logging.
    onRequest :: a -> Request -> IO ()
    onRequest _ _ = return ()

    -- | An absolute URL to the root of the application. Do not include
    -- trailing slash.
    approot :: a -> Approot

-- | A convenience wrapper around 'simpleApplyLayout for HTML-only data.
simpleApplyLayout :: Yesod y
                  => String -- ^ title
                  -> Hamlet (Routes y) IO () -- ^ body
                  -> Handler y ChooseRep
simpleApplyLayout t b = do
    let pc = PageContent
                { pageTitle = return $ Unencoded $ cs t
                , pageHead = return ()
                , pageBody = b
                }
    y <- getYesod
    rr <- getRequest
    content <- hamletToContent $ applyLayout y pc rr
    return $ chooseRep
        [ (TypeHtml, content)
        ]

getApproot :: Yesod y => Handler y Approot
getApproot = approot `fmap` getYesod

defaultErrorHandler :: Yesod y => ErrorResponse -> Handler y ChooseRep
defaultErrorHandler NotFound = do
    r <- waiRequest
    simpleApplyLayout "Not Found" $ [$hamlet|
%h1 Not Found
%p $helper$
|] r
  where
    helper = return . Unencoded . cs . W.pathInfo
defaultErrorHandler PermissionDenied =
    simpleApplyLayout "Permission Denied" $ [$hamlet|
%h1 Permission denied|] ()
defaultErrorHandler (InvalidArgs ia) =
    simpleApplyLayout "Invalid Arguments" $ [$hamlet|
%h1 Invalid Arguments
%dl
    $forall ias pair
        %dt $pair.key$
        %dd $pair.val$
|] ()
  where
    ias _ = return $ fromList $ map go ia
    go (k, v) = Pair (return $ Unencoded $ cs k)
                     (return $ Unencoded $ cs v)
defaultErrorHandler (InternalError e) =
    simpleApplyLayout "Internal Server Error" $ [$hamlet|
%h1 Internal Server Error
%p $message$
|] e
  where
    message :: String -> IO HtmlContent
    message = return . Unencoded . cs
defaultErrorHandler (BadMethod m) =
    simpleApplyLayout "Bad Method" $ [$hamlet|
%h1 Method Not Supported
%p Method "$m'$" not supported
|] ()
  where
    m' _ = return $ Unencoded $ cs m

data Pair m k v = Pair { key :: m k, val :: m v }

toWaiApp :: Yesod y => y -> IO W.Application
toWaiApp a = do
    key' <- encryptKey a
    let mins = clientSessionDuration a
    return $ gzip
           $ jsonp
           $ methodOverride
           $ cleanPath
           $ \thePath -> clientsession encryptedCookies key' mins
           $ toWaiApp' a thePath

toWaiApp' :: Yesod y
          => y
          -> [B.ByteString]
          -> [(B.ByteString, B.ByteString)]
          -> W.Request
          -> IO W.Response
toWaiApp' y resource session env = do
    let site = getSite
        method = B8.unpack $ W.methodToBS $ W.requestMethod env
        types = httpAccept env
        pathSegments = filter (not . null) $ cleanupSegments resource
        eurl = parsePathSegments site pathSegments
        render u = approot y ++ '/'
                 : encodePathInfo (fixSegs $ formatPathSegments site u)
    rr <- parseWaiRequest env session
    onRequest y rr
    print pathSegments
    let ya = case eurl of
                Left _ -> runHandler (errorHandler y NotFound) y render
                Right url -> handleSite site render url method
                                        (badMethod method) y
    let eh er = runHandler (errorHandler y er) y render
    unYesodApp ya eh rr types >>= responseToWaiResponse

cleanupSegments :: [B.ByteString] -> [String]
cleanupSegments = decodePathInfo . intercalate "/" . map B8.unpack

httpAccept :: W.Request -> [ContentType]
httpAccept = map contentTypeFromBS
           . parseHttpAccept
           . fromMaybe B.empty
           . lookup W.Accept
           . W.requestHeaders

-- | Runs an application with CGI if CGI variables are present (namely
-- PATH_INFO); otherwise uses SimpleServer.
basicHandler :: Int -- ^ port number
             -> W.Application -> IO ()
basicHandler port app = do
    vars <- getEnvironment
    case lookup "PATH_INFO" vars of
        Nothing -> do
            putStrLn $ "http://localhost:" ++ show port ++ "/"
            SS.run port app
        Just _ -> CGI.run app

badMethod :: String -> YesodApp
badMethod m = YesodApp $ \eh req cts
         -> unYesodApp (eh $ BadMethod m) eh req cts

fixSegs :: [String] -> [String]
fixSegs [] = []
fixSegs [x]
    | any (== '.') x = [x]
    | otherwise = [x, ""] -- append trailing slash
fixSegs (x:xs) = x : fixSegs xs
