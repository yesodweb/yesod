-- | The basic typeclass for a Yesod application.
module Yesod.Yesod
    ( Yesod (..)
    , YesodApproot (..)
    , applyLayout'
    , applyLayoutJson
    , getApproot
    , toHackApp
    ) where

import Data.Object.Html
import Data.Object.Json (unJsonDoc)
import Yesod.Response
import Yesod.Request
import Yesod.Definitions
import Yesod.Handler

import Data.Maybe (fromMaybe)
import Web.Mime
import Web.Encodings (parseHttpAccept)

import qualified Hack
import Hack.Middleware.CleanPath
import Hack.Middleware.ClientSession
import Hack.Middleware.Gzip
import Hack.Middleware.Jsonp
import Hack.Middleware.MethodOverride

class Yesod a where
    -- | Please use the Quasi-Quoter, you\'ll be happier. For more information,
    -- see the examples/fact.lhs sample.
    handlers :: Resource -> Verb -> Handler a ChooseRep

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
                -> String -- ^ title
                -> Html -- ^ body
                -> Content
    applyLayout _ t b = cs (cs (Tag "title" [] $ cs t, b) :: HtmlDoc)

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
    return $ chooseRep
        [ (TypeHtml, applyLayout y t b)
        ]

-- | A convenience wrapper around 'applyLayout' which provides a JSON
-- representation of the body.
applyLayoutJson :: Yesod y
                => String
                -> HtmlObject
                -> Handler y ChooseRep
applyLayoutJson t b = do
    y <- getYesod
    return $ chooseRep
        [ (TypeJson, cs $ unJsonDoc $ cs b)
        , (TypeHtml, applyLayout y t $ cs b)
        ]

getApproot :: YesodApproot y => Handler y Approot
getApproot = approot `fmap` getYesod

defaultErrorHandler :: Yesod y
                    => ErrorResponse
                    -> Handler y ChooseRep
defaultErrorHandler NotFound = do
    rr <- getRawRequest
    applyLayout' "Not Found" $ cs $ toHtmlObject [("Not found", show rr)]
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

toHackApp :: Yesod y => y -> IO Hack.Application
toHackApp a = do
    key <- encryptKey a
    let app' = toHackApp' a
    let mins = clientSessionDuration a
    return $ gzip
           $ cleanPath
           $ jsonp
           $ methodOverride
           $ clientsession encryptedCookies key mins
           $ app'

toHackApp' :: Yesod y => y -> Hack.Env -> IO Hack.Response
toHackApp' y env = do
    let (Right resource) = splitPath $ Hack.pathInfo env
        types = httpAccept env
        verb = cs $ Hack.requestMethod env
        handler = handlers resource verb
        rr = cs env
    res <- runHandler handler errorHandler rr y types
    responseToHackResponse res

httpAccept :: Hack.Env -> [ContentType]
httpAccept = map TypeOther . parseHttpAccept . fromMaybe ""
           . lookup "Accept" . Hack.http
