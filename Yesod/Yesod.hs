-- | The basic typeclass for a Yesod application.
module Yesod.Yesod
    ( Yesod (..)
    , toHackApp
    ) where

import Yesod.Rep
import Data.Object.Html (toHtmlObject)
import Yesod.Response
import Yesod.Request
import Yesod.Constants
import Yesod.Definitions
import Yesod.Resource
import Yesod.Handler

--import Control.Monad (when)

import qualified Hack
import Hack.Middleware.CleanPath
import Hack.Middleware.ClientSession
import Hack.Middleware.Gzip
import Hack.Middleware.Jsonp
import Hack.Middleware.MethodOverride

type ContentPair = (ContentType, Content)

class Yesod a where
    handlers ::
        [(ResourcePatternString,
          [(Verb, [ContentType] -> Handler a ContentPair)])]

    -- | The encryption key to be used for encrypting client sessions.
    encryptKey :: a -> IO Word256
    encryptKey _ = getKey defaultKeyFile

    -- | All of the middlewares to install.
    hackMiddleware :: a -> [Hack.Middleware]
    hackMiddleware _ =
                [ gzip
                , cleanPath
                , jsonp
                , methodOverride
                ]

    -- | Output error response pages.
    errorHandler :: ErrorResult -> [ContentType] -> Handler a ContentPair
    errorHandler = defaultErrorHandler

    -- | Whether or not we should check for overlapping resource names.
    checkOverlaps :: a -> Bool
    checkOverlaps = const True

    -- | An absolute URL to the root of the application.
    approot :: a -> Approot

defaultErrorHandler :: Yesod y
                    => ErrorResult
                    -> [ContentType]
                    -> Handler y ContentPair
defaultErrorHandler NotFound cts = do
    rr <- askRawRequest
    return $ chooseRep (toHtmlObject $ "Not found: " ++ show rr) cts
defaultErrorHandler (Redirect url) cts =
    return $ chooseRep (toHtmlObject $ "Redirect to: " ++ url) cts
defaultErrorHandler PermissionDenied cts =
    return $ chooseRep (toHtmlObject "Permission denied") cts
defaultErrorHandler (InvalidArgs ia) cts =
    return $ chooseRep (toHtmlObject
            [ ("errorMsg", toHtmlObject "Invalid arguments")
            , ("messages", toHtmlObject ia)
            ]) cts
defaultErrorHandler (InternalError e) cts =
    return $ chooseRep (toHtmlObject
                [ ("Internal server error", e)
                ]) cts

toHackApp :: Yesod y => y -> Hack.Application
toHackApp a env = do
    -- FIXME when (checkOverlaps a) $ checkResourceName a -- FIXME maybe this should be done compile-time?
    key <- encryptKey a
    let app' = toHackApp' a
        clientsession' = clientsession [authCookieName] key -- FIXME gotta be a better way...
        app = foldr ($) app' $ hackMiddleware a ++ [clientsession']
    app env

toHackApp' :: Yesod y => y -> Hack.Application
toHackApp' = undefined -- FIXME
