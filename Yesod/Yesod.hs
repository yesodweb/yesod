-- | The basic typeclass for a Yesod application.
module Yesod.Yesod
    ( Yesod (..)
    , YesodApproot (..)
    , getApproot
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
import Yesod.Template (TemplateGroup)

import Data.Maybe (fromMaybe)
import Data.Convertible.Text
import Text.StringTemplate
import Web.Mime

import qualified Hack
import Hack.Middleware.CleanPath
import Hack.Middleware.ClientSession
import Hack.Middleware.Gzip
import Hack.Middleware.Jsonp
import Hack.Middleware.MethodOverride

class Yesod a where
    -- | Please use the Quasi-Quoter, you\'ll be happier. For more information,
    -- see the examples/fact.lhs sample.
    handlers :: Resource -> Verb -> Handler a RepChooser

    -- | The encryption key to be used for encrypting client sessions.
    encryptKey :: a -> IO Word256
    encryptKey _ = getKey defaultKeyFile

    -- | Number of minutes before a client session times out. Defaults to
    -- 120 (2 hours).
    clientSessionDuration :: a -> Int
    clientSessionDuration = const 120

    -- | Output error response pages.
    errorHandler :: ErrorResult -> Handler a RepChooser
    errorHandler = defaultErrorHandler

    -- | The template directory. Blank means no templates.
    templateDir :: a -> FilePath
    templateDir _ = ""

class Yesod a => YesodApproot a where
    -- | An absolute URL to the root of the application.
    approot :: a -> Approot

getApproot :: YesodApproot y => Handler y Approot
getApproot = approot `fmap` getYesod

defaultErrorHandler :: Yesod y
                    => ErrorResult
                    -> Handler y RepChooser
defaultErrorHandler NotFound = do
    rr <- getRawRequest
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

toHackApp :: Yesod y => y -> IO Hack.Application
toHackApp a = do
    key <- encryptKey a
    app' <- toHackApp' a
    let mins = clientSessionDuration a
    return $ gzip
           $ cleanPath
           $ jsonp
           $ methodOverride
           $ clientsession encryptedCookies key mins
           $ app'

toHackApp' :: Yesod y => y -> IO Hack.Application
toHackApp' y = do
    let td = templateDir y
    tg <- if null td
            then return nullGroup
            else directoryGroupRecursiveLazy td
    return $ toHackApp'' y tg

toHackApp'' :: Yesod y => y -> TemplateGroup -> Hack.Env -> IO Hack.Response
toHackApp'' y tg env = do
    let (Right resource) = splitPath $ Hack.pathInfo env
        types = httpAccept env
        verb = cs $ Hack.requestMethod env
        handler = handlers resource verb
        rr = cs env
    res <- runHandler handler errorHandler rr y tg types
    let langs = ["en"] -- FIXME
    responseToHackResponse langs res

httpAccept :: Hack.Env -> [ContentType]
httpAccept = map TypeOther . parseHttpAccept . fromMaybe ""
           . lookup "Accept" . Hack.http
