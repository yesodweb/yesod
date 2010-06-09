{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | The basic typeclass for a Yesod application.
module Yesod.Yesod
    ( -- * Type classes
      Yesod (..)
    , YesodSite (..)
      -- ** Persistence
    , YesodPersist (..)
    , Persist (..)
      -- * Convenience functions
    , applyLayout
    , applyLayoutJson
      -- * Defaults
    , defaultErrorHandler
    ) where

import Yesod.Content
import Yesod.Request
import Yesod.Hamlet
import Yesod.Handler
import qualified Network.Wai as W
import Yesod.Json
import Yesod.Internal
import Web.ClientSession (getKey, defaultKeyFile)
import qualified Web.ClientSession as CS
import Data.Monoid (mempty)
import Data.ByteString.UTF8 (toString)
import Database.Persist (Persist (..))

import Web.Routes.Quasi (QuasiSite (..), Routes)

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function. You should never need to deal with it directly.
class YesodSite y where
    getSite :: QuasiSite YesodApp y y

-- | Define settings for a Yesod applications. The only required setting is
-- 'approot'; other than that, there are intelligent defaults.
class Yesod a where
    -- | An absolute URL to the root of the application. Do not include
    -- trailing slash.
    --
    -- If you want to be lazy, you can supply an empty string under the
    -- following conditions:
    --
    -- * Your application is served from the root of the domain.
    --
    -- * You do not use any features that require absolute URLs, such as Atom
    -- feeds and XML sitemaps.
    approot :: a -> String

    -- | The encryption key to be used for encrypting client sessions.
    encryptKey :: a -> IO CS.Key
    encryptKey _ = getKey defaultKeyFile

    -- | Number of minutes before a client session times out. Defaults to
    -- 120 (2 hours).
    clientSessionDuration :: a -> Int
    clientSessionDuration = const 120

    -- | Output error response pages.
    errorHandler :: Yesod y
                 => a
                 -> ErrorResponse
                 -> Handler y ChooseRep
    errorHandler _ = defaultErrorHandler

    -- | Applies some form of layout to the contents of a page.
    defaultLayout :: PageContent (Routes a) -> GHandler sub a Content
    defaultLayout p = hamletToContent [$hamlet|
!!!
%html
    %head
        %title $pageTitle.p$
        ^pageHead.p^
    %body
        ^pageBody.p^
|]

    -- | Gets called at the beginning of each request. Useful for logging.
    onRequest :: a -> Request -> IO ()
    onRequest _ _ = return ()

    -- | Override the rendering function for a particular URL. One use case for
    -- this is to offload static hosting to a different domain name to avoid
    -- sending cookies.
    urlRenderOverride :: a -> Routes a -> Maybe String
    urlRenderOverride _ _ = Nothing

    -- | Determine is a request is authorized or not.
    --
    -- Return 'Nothing' is the request is authorized, 'Just' a message if
    -- unauthorized. If authentication is required, you should use a redirect;
    -- the Auth helper provides this functionality automatically.
    isAuthorized :: a -> Routes a -> IO (Maybe String)
    isAuthorized _ _ = return Nothing

-- | Apply the default layout ('defaultLayout') to the given title and body.
applyLayout :: Yesod master
            => String -- ^ title
            -> Hamlet (Routes master) -- ^ head
            -> Hamlet (Routes master) -- ^ body
            -> GHandler sub master RepHtml
applyLayout t h b =
    RepHtml `fmap` defaultLayout PageContent
                { pageTitle = string t
                , pageHead = h
                , pageBody = b
                }

-- | Provide both an HTML and JSON representation for a piece of data, using
-- the default layout for the HTML output ('defaultLayout').
applyLayoutJson :: Yesod master
                => String -- ^ title
                -> Hamlet (Routes master) -- ^ head
                -> Hamlet (Routes master) -- ^ body
                -> Json
                -> GHandler sub master RepHtmlJson
applyLayoutJson t h html json = do
    html' <- defaultLayout PageContent
                { pageTitle = string t
                , pageHead = h
                , pageBody = html
                }
    json' <- jsonToContent json
    return $ RepHtmlJson html' json'

applyLayout' :: Yesod master
             => String -- ^ title
             -> Hamlet (Routes master) -- ^ body
             -> GHandler sub master ChooseRep
applyLayout' s = fmap chooseRep . applyLayout s mempty

-- | The default error handler for 'errorHandler'.
defaultErrorHandler :: Yesod y
                    => ErrorResponse
                    -> Handler y ChooseRep
defaultErrorHandler NotFound = do
    r <- waiRequest
    applyLayout' "Not Found" $ [$hamlet|
%h1 Not Found
%p $string.toString.pathInfo.r$
|]
  where
    pathInfo = W.pathInfo
defaultErrorHandler (PermissionDenied msg) =
    applyLayout' "Permission Denied" $ [$hamlet|
%h1 Permission denied
%p $string.msg$
|]
defaultErrorHandler (InvalidArgs ia) =
    applyLayout' "Invalid Arguments" $ [$hamlet|
%h1 Invalid Arguments
%dl
    $forall ia pair
        %dt $string.fst.pair$
        %dd $string.snd.pair$
|]
defaultErrorHandler (InternalError e) =
    applyLayout' "Internal Server Error" $ [$hamlet|
%h1 Internal Server Error
%p $string.e$
|]
defaultErrorHandler (BadMethod m) =
    applyLayout' "Bad Method" $ [$hamlet|
%h1 Method Not Supported
%p Method "$string.m$" not supported
|]

class YesodPersist y where
    type YesodDB y :: (* -> *) -> * -> *
    runDB :: YesodDB y (GHandler sub y) a -> GHandler sub y a
