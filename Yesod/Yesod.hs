-- | The basic typeclass for a Yesod application.
module Yesod.Yesod
    ( Yesod (..)
    , YesodApproot (..)
    , toHackApp
    ) where

import Yesod.Rep
import Data.Object.Html (toHtmlObject)
import Yesod.Response
import Yesod.Request
import Yesod.Constants
import Yesod.Definitions
import Yesod.Handler
import Yesod.Utils

import Data.Maybe (fromMaybe)
import Data.Convertible.Text

import qualified Hack
import Hack.Middleware.CleanPath
import Hack.Middleware.ClientSession
import Hack.Middleware.Gzip
import Hack.Middleware.Jsonp
import Hack.Middleware.MethodOverride

class Yesod a where
    -- | Please use the Quasi-Quoter, you\'ll be happier. FIXME more info.
    handlers :: Resource -> Verb -> Handler a RepChooser

    -- | The encryption key to be used for encrypting client sessions.
    encryptKey :: a -> IO Word256
    encryptKey _ = getKey defaultKeyFile

    -- | Output error response pages.
    errorHandler :: ErrorResult -> Handler a RepChooser
    errorHandler = defaultErrorHandler

class Yesod a => YesodApproot a where
    -- | An absolute URL to the root of the application.
    approot :: a -> Approot

defaultErrorHandler :: Yesod y
                    => ErrorResult
                    -> Handler y RepChooser
defaultErrorHandler NotFound = do
    rr <- askRawRequest
    return $ chooseRep $ toHtmlObject $ "Not found: " ++ show rr
defaultErrorHandler (Redirect url) =
    return $ chooseRep $ toHtmlObject $ "Redirect to: " ++ url
defaultErrorHandler PermissionDenied =
    return $ chooseRep $ toHtmlObject "Permission denied"
defaultErrorHandler (InvalidArgs ia) =
    return $ chooseRep $ toHtmlObject
            [ ("errorMsg", toHtmlObject "Invalid arguments")
            , ("messages", toHtmlObject ia)
            ]
defaultErrorHandler (InternalError e) =
    return $ chooseRep $ toHtmlObject
                [ ("Internal server error", e)
                ]

toHackApp :: Yesod y => y -> Hack.Application
toHackApp a env = do
    key <- encryptKey a
    let app' = toHackApp' a
        middleware =
                [ gzip
                , cleanPath
                , jsonp
                , methodOverride
                , clientsession [authCookieName] key
                ]
        app = foldr ($) app' middleware
    app env

toHackApp' :: Yesod y => y -> Hack.Application
toHackApp' y env = do
    let (Right resource) = splitPath $ Hack.pathInfo env
        types = httpAccept env
        verb = cs $ Hack.requestMethod env
        handler = handlers resource verb
        rr = cs env
    res <- runHandler handler errorHandler rr y types
    let langs = ["en"] -- FIXME
    responseToHackResponse langs res

httpAccept :: Hack.Env -> [ContentType]
httpAccept = map TypeOther . parseHttpAccept . fromMaybe ""
           . lookup "Accept" . Hack.http
