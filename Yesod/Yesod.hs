{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | The basic typeclass for a Yesod application.
module Yesod.Yesod
    ( -- * Type classes
      Yesod (..)
    , YesodSite (..)
    , YesodSubSite (..)
      -- ** Persistence
    , YesodPersist (..)
    , module Database.Persist
      -- ** Breadcrumbs
    , YesodBreadcrumbs (..)
    , breadcrumbs
      -- * Convenience functions
    , applyLayout
    , applyLayoutJson
    , maybeAuthorized
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
import Database.Persist
import Web.Routes.Site (Site)
import Data.Maybe (isNothing)

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function. You should never need to deal with it directly.
class YesodSite y where
    getSite :: Site (Routes y) (Method -> Maybe (Handler y ChooseRep))
type Method = String

-- | Same as 'YesodSite', but for subsites. Once again, users should not need
-- to deal with it directly, as the mkYesodSub creates instances appropriately.
class YesodSubSite s y where
    getSubSite :: Site (Routes s) (Method -> Maybe (GHandler s y ChooseRep))

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
    errorHandler :: ErrorResponse -> GHandler sub a ChooseRep
    errorHandler = defaultErrorHandler

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
    onRequest :: GHandler sub a ()
    onRequest = return ()

    -- | Override the rendering function for a particular URL. One use case for
    -- this is to offload static hosting to a different domain name to avoid
    -- sending cookies.
    urlRenderOverride :: a -> Routes a -> Maybe String
    urlRenderOverride _ _ = Nothing

    -- | Determine if a request is authorized or not.
    --
    -- Return 'Nothing' is the request is authorized, 'Just' a message if
    -- unauthorized. If authentication is required, you should use a redirect;
    -- the Auth helper provides this functionality automatically.
    isAuthorized :: Routes a -> GHandler s a (Maybe String) -- FIXME use a data type that specifies whether authentication is required
    isAuthorized _ = return Nothing

-- | A type-safe, concise method of creating breadcrumbs for pages. For each
-- resource, you declare the title of the page and the parent resource (if
-- present).
class YesodBreadcrumbs y where
    -- | Returns the title and the parent resource, if available. If you return
    -- a 'Nothing', then this is considered a top-level page.
    breadcrumb :: Routes y -> Handler y (String, Maybe (Routes y))

-- | Gets the title of the current page and the hierarchy of parent pages,
-- along with their respective titles.
breadcrumbs :: YesodBreadcrumbs y => Handler y (String, [(Routes y, String)])
breadcrumbs = do
    x <- getRoute
    case x of
        Nothing -> return ("Not found", [])
        Just y -> do
            (title, next) <- breadcrumb y
            z <- go [] next
            return (title, z)
  where
    go back Nothing = return back
    go back (Just this) = do
        (title, next) <- breadcrumb this
        go ((this, title) : back) next

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
defaultErrorHandler :: Yesod y => ErrorResponse -> GHandler sub y ChooseRep
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
%ul
    $forall ia msg
        %li $string.msg$
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

-- | Return the same URL if the user is authorized to see it.
--
-- Built on top of 'isAuthorized'. This is useful for building page that only
-- contain links to pages the user is allowed to see.
maybeAuthorized :: Yesod a => Routes a -> GHandler s a (Maybe (Routes a))
maybeAuthorized r = do
    x <- isAuthorized r
    return $ if isNothing x then Just r else Nothing
