{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | The basic typeclass for a Yesod application.
module Yesod.Yesod
    ( -- * Type classes
      Yesod (..)
    , YesodSite (..)
    , YesodSubSite (..)
      -- ** Persistence
    , YesodPersist (..)
    , module Database.Persist
    , get404
      -- ** Breadcrumbs
    , YesodBreadcrumbs (..)
    , breadcrumbs
      -- * Utitlities
    , maybeAuthorized
    , widgetToPageContent
    , defaultLayoutJson
    , redirectToPost
      -- * Defaults
    , defaultErrorHandler
      -- * Data types
    , AuthResult (..)
#if TEST
    , testSuite
#endif
    ) where

#if TEST
import Yesod.Content hiding (testSuite)
import Yesod.Json hiding (testSuite)
import Yesod.Handler hiding (testSuite)
#else
import Yesod.Content
import Yesod.Json
import Yesod.Handler
#endif

import Yesod.Widget
import Yesod.Request
import Yesod.Hamlet
import qualified Network.Wai as W
import Yesod.Internal
import Web.ClientSession (getKey, defaultKeyFile)
import qualified Web.ClientSession as CS
import qualified Data.ByteString.UTF8 as BSU
import Database.Persist
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Failure (Failure)
import qualified Data.ByteString as S
import qualified Network.Wai.Middleware.CleanPath
import qualified Data.ByteString.Lazy as L
import Yesod.WebRoutes
import Data.Monoid
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State hiding (get)
import Text.Hamlet
import Text.Cassius
import Text.Julius

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
#endif

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function. You should never need to deal with it directly.
class Eq (Route y) => YesodSite y where
    getSite :: Site (Route y) (Method -> Maybe (GHandler y y ChooseRep))
type Method = String

-- | Same as 'YesodSite', but for subsites. Once again, users should not need
-- to deal with it directly, as the mkYesodSub creates instances appropriately.
class Eq (Route s) => YesodSubSite s y where
    getSubSite :: Site (Route s) (Method -> Maybe (GHandler s y ChooseRep))

-- | Define settings for a Yesod applications. The only required setting is
-- 'approot'; other than that, there are intelligent defaults.
class Eq (Route a) => Yesod a where
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
    defaultLayout :: GWidget sub a () -> GHandler sub a RepHtml
    defaultLayout w = do
        p <- widgetToPageContent w
        mmsg <- getMessage
        hamletToRepHtml [$hamlet|
!!!
%html
    %head
        %title $pageTitle.p$
        ^pageHead.p^
    %body
        $maybe mmsg msg
            %p.message $msg$
        ^pageBody.p^
|]

    -- | Gets called at the beginning of each request. Useful for logging.
    onRequest :: GHandler sub a ()
    onRequest = return ()

    -- | Override the rendering function for a particular URL. One use case for
    -- this is to offload static hosting to a different domain name to avoid
    -- sending cookies.
    urlRenderOverride :: a -> Route a -> Maybe String
    urlRenderOverride _ _ = Nothing

    -- | Determine if a request is authorized or not.
    --
    -- Return 'Nothing' is the request is authorized, 'Just' a message if
    -- unauthorized. If authentication is required, you should use a redirect;
    -- the Auth helper provides this functionality automatically.
    isAuthorized :: Route a
                 -> Bool -- ^ is this a write request?
                 -> GHandler s a AuthResult
    isAuthorized _ _ = return Authorized

    -- | Determines whether the current request is a write request. By default,
    -- this assumes you are following RESTful principles, and determines this
    -- from request method. In particular, all except the following request
    -- methods are considered write: GET HEAD OPTIONS TRACE.
    --
    -- This function is used to determine if a request is authorized; see
    -- 'isAuthorized'.
    isWriteRequest :: Route a -> GHandler s a Bool
    isWriteRequest _ = do
        wai <- waiRequest
        return $ not $ W.requestMethod wai `elem`
            ["GET", "HEAD", "OPTIONS", "TRACE"]

    -- | The default route for authentication.
    --
    -- Used in particular by 'isAuthorized', but library users can do whatever
    -- they want with it.
    authRoute :: a -> Maybe (Route a)
    authRoute _ = Nothing

    -- | A function used to split a raw PATH_INFO value into path pieces. It
    -- returns a 'Left' value when you should redirect to the given path, and a
    -- 'Right' value on successful parse.
    --
    -- By default, it splits paths on slashes, and ensures the following are true:
    --
    -- * No double slashes
    --
    -- * If the last path segment has a period, there is no trailing slash.
    --
    -- * Otherwise, ensures there /is/ a trailing slash.
    splitPath :: a -> S.ByteString -> Either S.ByteString [String]
    splitPath _ = Network.Wai.Middleware.CleanPath.splitPath

    -- | Join the pieces of a path together into an absolute URL. This should
    -- be the inverse of 'splitPath'.
    joinPath :: a -> String -> [String] -> [(String, String)] -> String
    joinPath _ ar pieces qs =
        ar ++ '/' : encodePathInfo (fixSegs pieces) qs
      where
        fixSegs [] = []
        fixSegs [x]
            | any (== '.') x = [x]
            | otherwise = [x, ""] -- append trailing slash
        fixSegs (x:xs) = x : fixSegs xs

    -- | This function is used to store some static content to be served as an
    -- external file. The most common case of this is stashing CSS and
    -- JavaScript content in an external file; the "Yesod.Widget" module uses
    -- this feature.
    --
    -- The return value is 'Nothing' if no storing was performed; this is the
    -- default implementation. A 'Just' 'Left' gives the absolute URL of the
    -- file, whereas a 'Just' 'Right' gives the type-safe URL. The former is
    -- necessary when you are serving the content outside the context of a
    -- Yesod application, such as via memcached.
    addStaticContent :: String -- ^ filename extension
                     -> String -- ^ mime-type
                     -> L.ByteString -- ^ content
                     -> GHandler sub a (Maybe (Either String (Route a, [(String, String)])))
    addStaticContent _ _ _ = return Nothing

data AuthResult = Authorized | AuthenticationRequired | Unauthorized String
    deriving (Eq, Show, Read)

-- | A type-safe, concise method of creating breadcrumbs for pages. For each
-- resource, you declare the title of the page and the parent resource (if
-- present).
class YesodBreadcrumbs y where
    -- | Returns the title and the parent resource, if available. If you return
    -- a 'Nothing', then this is considered a top-level page.
    breadcrumb :: Route y -> GHandler sub y (String, Maybe (Route y))

-- | Gets the title of the current page and the hierarchy of parent pages,
-- along with their respective titles.
breadcrumbs :: YesodBreadcrumbs y => GHandler sub y (String, [(Route y, String)])
breadcrumbs = do
    x' <- getCurrentRoute
    tm <- getRouteToMaster
    let x = fmap tm x'
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

-- | Provide both an HTML and JSON representation for a piece of data, using
-- the default layout for the HTML output ('defaultLayout').
defaultLayoutJson :: Yesod master
                  => GWidget sub master ()
                  -> Json
                  -> GHandler sub master RepHtmlJson
defaultLayoutJson w json = do
    RepHtml html' <- defaultLayout w
    json' <- jsonToContent json
    return $ RepHtmlJson html' json'

applyLayout' :: Yesod master
             => Html -- ^ title
             -> Hamlet (Route master) -- ^ body
             -> GHandler sub master ChooseRep
applyLayout' title body = fmap chooseRep $ defaultLayout $ do
    setTitle title
    addBody body

-- | The default error handler for 'errorHandler'.
defaultErrorHandler :: Yesod y => ErrorResponse -> GHandler sub y ChooseRep
defaultErrorHandler NotFound = do
    r <- waiRequest
    let path' = BSU.toString $ pathInfo r
    applyLayout' "Not Found" $ [$hamlet|
%h1 Not Found
%p $path'$
|]
  where
    pathInfo = W.pathInfo
defaultErrorHandler (PermissionDenied msg) =
    applyLayout' "Permission Denied" $ [$hamlet|
%h1 Permission denied
%p $msg$
|]
defaultErrorHandler (InvalidArgs ia) =
    applyLayout' "Invalid Arguments" $ [$hamlet|
%h1 Invalid Arguments
%ul
    $forall ia msg
        %li $msg$
|]
defaultErrorHandler (InternalError e) =
    applyLayout' "Internal Server Error" $ [$hamlet|
%h1 Internal Server Error
%p $e$
|]
defaultErrorHandler (BadMethod m) =
    applyLayout' "Bad Method" $ [$hamlet|
%h1 Method Not Supported
%p Method "$m$" not supported
|]

class YesodPersist y where
    type YesodDB y :: (* -> *) -> * -> *
    runDB :: YesodDB y (GHandler sub y) a -> GHandler sub y a


-- Get the given entity by ID, or return a 404 not found if it doesn't exist.
get404 :: (PersistBackend (t m), PersistEntity val, Monad (t m),
           Failure ErrorResponse m, MonadTrans t)
       => Key val -> t m val
get404 key = do
    mres <- get key
    case mres of
        Nothing -> lift notFound
        Just res -> return res

-- | Return the same URL if the user is authorized to see it.
--
-- Built on top of 'isAuthorized'. This is useful for building page that only
-- contain links to pages the user is allowed to see.
maybeAuthorized :: Yesod a
                => Route a
                -> Bool -- ^ is this a write request?
                -> GHandler s a (Maybe (Route a))
maybeAuthorized r isWrite = do
    x <- isAuthorized r isWrite
    return $ if x == Authorized then Just r else Nothing

-- | Convert a widget to a 'PageContent'.
widgetToPageContent :: (Eq (Route master), Yesod master)
                    => GWidget sub master ()
                    -> GHandler sub master (PageContent (Route master))
widgetToPageContent (GWidget w) = do
    w' <- flip evalStateT 0
        $ runWriterT $ runWriterT $ runWriterT $ runWriterT
        $ runWriterT $ runWriterT $ runWriterT w
    let ((((((((),
         Body body),
         Last mTitle),
         scripts'),
         stylesheets'),
         style),
         jscript),
         Head head') = w'
    let title = maybe mempty unTitle mTitle
    let scripts = map (locationToHamlet . unScript) $ runUniqueList scripts'
    let stylesheets = map (locationToHamlet . unStylesheet)
                    $ runUniqueList stylesheets'
    let cssToHtml (Css b) = Html b
        celper :: Cassius url -> Hamlet url
        celper = fmap cssToHtml
        jsToHtml (Javascript b) = Html b
        jelper :: Julius url -> Hamlet url
        jelper = fmap jsToHtml

    render <- getUrlRenderParams
    let renderLoc x =
            case x of
                Nothing -> Nothing
                Just (Left s) -> Just s
                Just (Right (u, p)) -> Just $ render u p
    cssLoc <-
        case style of
            Nothing -> return Nothing
            Just s -> do
                x <- addStaticContent "css" "text/css; charset=utf-8"
                   $ renderCassius render s
                return $ renderLoc x
    jsLoc <-
        case jscript of
            Nothing -> return Nothing
            Just s -> do
                x <- addStaticContent "js" "text/javascript; charset=utf-8"
                   $ renderJulius render s
                return $ renderLoc x

    let head'' = [$hamlet|
$forall scripts s
    %script!src=^s^
$forall stylesheets s
    %link!rel=stylesheet!href=^s^
$maybe style s
    $maybe cssLoc s
        %link!rel=stylesheet!href=$s$
    $nothing
        %style ^celper.s^
$maybe jscript j
    $maybe jsLoc s
        %script!src=$s$
    $nothing
        %script ^jelper.j^
^head'^
|]
    return $ PageContent title head'' body

#if TEST
testSuite :: Test
testSuite = testGroup "Yesod.Yesod"
    [ testProperty "join/split path" propJoinSplitPath
    , testCase "utf8 split path" caseUtf8SplitPath
    , testCase "utf8 join path" caseUtf8JoinPath
    ]

data TmpYesod = TmpYesod
data TmpRoute = TmpRoute deriving Eq
type instance Route TmpYesod = TmpRoute
instance Yesod TmpYesod where approot _ = ""

propJoinSplitPath :: [String] -> Bool
propJoinSplitPath ss =
    splitPath TmpYesod (BSU.fromString $ joinPath TmpYesod "" ss' [])
        == Right ss'
  where
    ss' = filter (not . null) ss

caseUtf8SplitPath :: Assertion
caseUtf8SplitPath = do
    Right ["שלום"] @=?
        splitPath TmpYesod (BSU.fromString "/שלום/")
    Right ["page", "Fooé"] @=?
        splitPath TmpYesod (BSU.fromString "/page/Fooé/")
    Right ["\156"] @=?
        splitPath TmpYesod (BSU.fromString "/\156/")
    Right ["ð"] @=?
        splitPath TmpYesod (BSU.fromString "/%C3%B0/")

caseUtf8JoinPath :: Assertion
caseUtf8JoinPath = do
    "/%D7%A9%D7%9C%D7%95%D7%9D/" @=? joinPath TmpYesod "" ["שלום"] []
#endif

-- | Redirect to a POST resource.
--
-- This is not technically a redirect; instead, it returns an HTML page with a
-- POST form, and some Javascript to automatically submit the form. This can be
-- useful when you need to post a plain link somewhere that needs to cause
-- changes on the server.
redirectToPost :: Route master -> GHandler sub master a
redirectToPost dest = hamletToRepHtml [$hamlet|
!!!
%html
    %head
        %title Redirecting...
    %body!onload="document.getElementById('form').submit()"
        %form#form!method=post!action=@dest@
            %noscript
                %p Javascript has been disabled; please click on the button below to be redirected.
            %input!type=submit!value=Continue
|] >>= sendResponse
