-- | The basic typeclass for a Yesod application.
module Yesod.Yesod
    ( Yesod (..)
    , YesodApproot (..)
    , applyLayout'
    , applyLayoutJson
    , getApproot
    , toWaiApp
    , basicHandler
    ) where

import Data.Object.Html
import Data.Object.Json (unJsonDoc)
import Yesod.Response
import Yesod.Request
import Yesod.Definitions
import Yesod.Handler
import qualified Data.ByteString as B

import Data.Maybe (fromMaybe)
import Web.Mime
import Web.Encodings (parseHttpAccept)

import qualified Network.Wai as W
import Network.Wai.Middleware.CleanPath
import Network.Wai.Middleware.ClientSession
import Network.Wai.Middleware.Jsonp
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Middleware.Gzip

import qualified Network.Wai.Handler.SimpleServer as SS
import qualified Network.Wai.Handler.CGI as CGI
import System.Environment (getEnvironment)

class Yesod a where
    -- | Please use the Quasi-Quoter, you\'ll be happier. For more information,
    -- see the examples/fact.lhs sample.
    resources :: Resource -> W.Method -> Handler a ChooseRep

    -- | The encryption key to be used for encrypting client sessions.
    encryptKey :: a -> IO Word256
    encryptKey _ = getKey defaultKeyFile

    -- | Number of minutes before a client session times out. Defaults to
    -- 120 (2 hours).
    clientSessionDuration :: a -> Int
    clientSessionDuration = const 120

    -- | Output error response pages.
    errorHandler :: ErrorResponse -> Handler a ChooseRep
    errorHandler = defaultErrorHandler

    -- | Applies some form of layout to <title> and <body> contents of a page.
    applyLayout :: a
                -> Request
                -> String -- ^ title
                -> Html -- ^ body
                -> Content
    applyLayout _ _ t b = cs (cs (Tag "title" [] $ cs t, b) :: HtmlDoc)

    -- | Gets called at the beginning of each request. Useful for logging.
    onRequest :: a -> Request -> IO ()
    onRequest _ _ = return ()

class Yesod a => YesodApproot a where
    -- | An absolute URL to the root of the application.
    approot :: a -> Approot

-- | A convenience wrapper around 'applyLayout'.
applyLayout' :: Yesod y
             => String
             -> Html
             -> Handler y ChooseRep
applyLayout' t b = do
    y <- getYesod
    rr <- getRequest
    return $ chooseRep
        [ (TypeHtml, applyLayout y rr t b)
        ]

-- | A convenience wrapper around 'applyLayout' which provides a JSON
-- representation of the body.
applyLayoutJson :: Yesod y
                => String
                -> HtmlObject
                -> Handler y ChooseRep
applyLayoutJson t b = do
    y <- getYesod
    rr <- getRequest
    return $ chooseRep
        [ (TypeHtml, applyLayout y rr t $ cs b)
        , (TypeJson, cs $ unJsonDoc $ cs b)
        ]

getApproot :: YesodApproot y => Handler y Approot
getApproot = approot `fmap` getYesod

defaultErrorHandler :: Yesod y
                    => ErrorResponse
                    -> Handler y ChooseRep
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
    let types = httpAccept env
        handler = resources (map cs resource) $ W.requestMethod env
    rr <- parseWaiRequest env session
    onRequest y rr
    res <- runHandler handler errorHandler rr y types
    responseToWaiResponse res

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
