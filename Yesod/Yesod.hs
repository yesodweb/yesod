{-# LANGUAGE QuasiQuotes #-}
-- | The basic typeclass for a Yesod application.
module Yesod.Yesod
    ( Yesod (..)
    , YesodSite (..)
    , applyLayout'
    , applyLayoutJson
    , getApproot
    , toWaiApp
    , basicHandler
    , hamletToContent -- FIXME put elsewhere
    , hamletToRepHtml
    ) where

import Data.Object.Html
import Data.Object.Json (unJsonDoc)
import Yesod.Response
import Yesod.Request
import Yesod.Definitions
import Yesod.Handler hiding (badMethod)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import Data.Maybe (fromMaybe)
import Web.Mime
import Web.Encodings (parseHttpAccept)
import Web.Routes (Site (..), encodePathInfo, decodePathInfo)
import Data.List (intercalate)
import Text.Hamlet
import Text.Hamlet.Monad (outputHtml)

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

data PageContent url = PageContent
    { pageTitle :: Hamlet url IO HtmlContent
    , pageHead :: Hamlet url IO ()
    , pageBody :: Hamlet url IO ()
    }

simpleContent :: String -> HtmlContent -> PageContent url
simpleContent title body = PageContent
    { pageTitle = return $ Unencoded $ cs title
    , pageHead = return ()
    , pageBody = outputHtml body
    }

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

    -- | Applies some form of layout to <title> and <body> contents of a page.
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

-- | A convenience wrapper around 'applyLayout'.
applyLayout' :: Yesod y
             => String
             -> Html
             -> Handler y ChooseRep
applyLayout' t b = do
    let pc = simpleContent t $ Encoded $ cs $ unHtmlFragment $ cs b
    y <- getYesod
    rr <- getRequest
    content <- hamletToContent $ applyLayout y pc rr
    return $ chooseRep
        [ (TypeHtml, content)
        ]

-- | A convenience wrapper around 'applyLayout' which provides a JSON
-- representation of the body.
applyLayoutJson :: Yesod y
                => String
                -> HtmlObject
                -> Handler y ChooseRep
applyLayoutJson t b = do
    let pc = simpleContent t $ Encoded $ cs $ unHtmlFragment
           $ cs (cs b :: Html)
    y <- getYesod
    rr <- getRequest
    htmlcontent <- hamletToContent $ applyLayout y pc rr
    return $ chooseRep
        [ (TypeHtml, htmlcontent)
        , (TypeJson, cs $ unJsonDoc $ cs b)
        ]

hamletToContent :: Hamlet (Routes y) IO () -> Handler y Content
hamletToContent h = do
    render <- getUrlRender
    return $ ContentEnum $ go render
  where
    go render iter seed = do
        res <- runHamlet h render seed $ iter' iter
        case res of
            Left x -> return $ Left x
            Right ((), x) -> return $ Right x
    iter' iter seed text = iter seed $ cs text

getApproot :: Yesod y => Handler y Approot
getApproot = approot `fmap` getYesod

defaultErrorHandler :: Yesod y => ErrorResponse -> Handler y ChooseRep
defaultErrorHandler NotFound = do
    r <- waiRequest
    applyLayout' "Not Found" $ cs $ toHtmlObject
        [ ("Not found", cs $ W.pathInfo r :: String)
        ]
defaultErrorHandler PermissionDenied =
    applyLayout' "Permission Denied" $ cs "Permission denied"
defaultErrorHandler (InvalidArgs ia) =
    applyLayout' "Invalid Arguments" $ cs $ toHtmlObject
            [ ("errorMsg", toHtmlObject "Invalid arguments")
            , ("messages", toHtmlObject ia)
            ]
defaultErrorHandler (InternalError e) =
    applyLayout' "Internal Server Error" $ cs $ toHtmlObject
        [ ("Internal server error", e)
        ]
defaultErrorHandler BadMethod =
    applyLayout' "Bad Method" $ cs "Method Not Supported"

toWaiApp :: Yesod y => y -> IO W.Application
toWaiApp a = do
    key <- encryptKey a
    let mins = clientSessionDuration a
    return $ gzip
           $ jsonp
           $ methodOverride
           $ cleanPath
           $ \thePath -> clientsession encryptedCookies key mins
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
                Right url -> handleSite site render url method badMethod y
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

badMethod :: YesodApp
badMethod = YesodApp $ \eh req cts -> unYesodApp (eh BadMethod) eh req cts

hamletToRepHtml :: Hamlet (Routes y) IO () -> Handler y RepHtml
hamletToRepHtml h = do
    c <- hamletToContent h
    return $ RepHtml c

fixSegs :: [String] -> [String]
fixSegs [] = []
fixSegs [x]
    | any (== '.') x = [x]
    | otherwise = [x, ""] -- append trailing slash
fixSegs (x:xs) = x : fixSegs xs
