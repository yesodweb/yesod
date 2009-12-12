-- | The basic typeclass for a Yesod application.
module Yesod.Yesod
    ( Yesod (..)
    , Handler
    , toHackApp
    ) where

import Yesod.Rep
import Data.Object.Html (toHtmlObject)
import Yesod.Response hiding (reps, ContentType, Content, chooseRep)
import Yesod.Request
import Yesod.Constants
--import Yesod.Definitions
--import Yesod.Resource (checkResourceName)

import Control.Applicative
--import Control.Monad (when)

import qualified Hack
import Hack.Middleware.CleanPath
import Hack.Middleware.ClientSession
import Hack.Middleware.Gzip
import Hack.Middleware.Jsonp
import Hack.Middleware.MethodOverride

type Handler a v = a -> IO v -- FIXME
type HandlerMap a = [(String, [ContentType] -> Handler a Content)]

class Yesod a where
    handlers :: HandlerMap a

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
    errorHandler :: a -> RawRequest -> ErrorResult -> [ContentType] -> MyIdentity (ContentType, Content) -- FIXME better type sig?
    errorHandler = defaultErrorHandler
    -- | Whether or not we should check for overlapping resource names.
    checkOverlaps :: a -> Bool
    checkOverlaps = const True

newtype MyIdentity a = MyIdentity { _unMyIdentity :: a }
instance Functor MyIdentity where
    fmap f (MyIdentity a) = MyIdentity $ f a
instance Applicative MyIdentity where
    pure = MyIdentity
    (MyIdentity f) <*> (MyIdentity a) = MyIdentity $ f a

defaultErrorHandler :: a
                    -> RawRequest
                    -> ErrorResult
                    -> [ContentType]
                    -> MyIdentity (ContentType, Content)
defaultErrorHandler _ rr NotFound = chooseRep $ pure . toHtmlObject $
                                   "Not found: " ++ show rr
defaultErrorHandler _ _ (Redirect url) =
        chooseRep $ pure . toHtmlObject $ "Redirect to: " ++ url
defaultErrorHandler _ _ (InternalError e) =
        chooseRep $ pure . toHtmlObject $ "Internal server error: " ++ e
defaultErrorHandler _ _ (InvalidArgs ia) =
        chooseRep $ pure $ toHtmlObject
            [ ("errorMsg", toHtmlObject "Invalid arguments")
            , ("messages", toHtmlObject ia)
            ]
defaultErrorHandler _ _ PermissionDenied =
        chooseRep $ pure $ toHtmlObject "Permission denied"

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
