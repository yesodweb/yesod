{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | The basic typeclass for a Yesod application.
module Yesod.Core
    ( -- * Type classes
      Yesod (..)
    , YesodSite (..)
    , YesodSubSite (..)
      -- ** Breadcrumbs
    , YesodBreadcrumbs (..)
    , breadcrumbs
      -- * Utitlities
    , maybeAuthorized
    , widgetToPageContent
      -- * Defaults
    , defaultErrorHandler
      -- * Data types
    , AuthResult (..)
      -- * Misc
    , yesodVersion
    , yesodRender
#if TEST
    , coreTestSuite
#endif
    ) where

import Yesod.Content
import Yesod.Handler

import qualified Paths_yesod_core
import Data.Version (showVersion)
import Yesod.Widget
import Yesod.Request
import qualified Network.Wai as W
import Yesod.Internal
import Yesod.Internal.Session
import Yesod.Internal.Request
import Web.ClientSession (getKey, defaultKeyFile)
import qualified Web.ClientSession as CS
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Monoid
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State hiding (get, put)
import Text.Hamlet
import Text.Cassius
import Text.Julius
import Web.Routes
import Text.Blaze (preEscapedLazyText)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Web.Cookie (parseCookies)
import qualified Data.Map as Map
import Data.Time

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import qualified Data.Text
import qualified Data.Text.Encoding
#endif

#if GHC7
#define HAMLET hamlet
#else
#define HAMLET $hamlet
#endif

-- FIXME ditch the whole Site thing and just have render and dispatch?

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function. You should never need to deal with it directly.
class Eq (Route y) => YesodSite y where
    getSite :: Site (Route y) (Method -> Maybe (GHandler y y ChooseRep))
    getSite' :: y -> Site (Route y) (Method -> Maybe (GHandler y y ChooseRep))
    getSite' _ = getSite
    dispatchToSubsite :: y -> Maybe CS.Key -> [String] -> Maybe W.Application

type Method = String

-- | Same as 'YesodSite', but for subsites. Once again, users should not need
-- to deal with it directly, as mkYesodSub creates instances appropriately.
class Eq (Route s) => YesodSubSite s y where
    getSubSite :: Site (Route s) (Method -> Maybe (GHandler s y ChooseRep))
    getSubSite' :: s -> y -> Site (Route s) (Method -> Maybe (GHandler s y ChooseRep))
    getSubSite' _ _ = getSubSite
    dispatchSubsite :: (Yesod y, YesodSite y)
                    => y
                    -> Maybe CS.Key
                    -> [String]
                    -> (Route s -> Route y)
                    -> s
                    -> W.Application
    dispatchToSubSubsite
        :: (Yesod y, YesodSite y)
        => y
        -> Maybe CS.Key
        -> [String]
        -> (Route s -> Route y)
        -> s
        -> Maybe W.Application

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
    --
    -- FIXME: is this the right typesig?
    approot :: a -> S.ByteString

    -- | The encryption key to be used for encrypting client sessions.
    -- Returning 'Nothing' disables sessions.
    encryptKey :: a -> IO (Maybe CS.Key)
    encryptKey _ = fmap Just $ getKey defaultKeyFile

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
        hamletToRepHtml [HAMLET|
!!!

<html>
    <head>
        <title>#{pageTitle p}
        ^{pageHead p}
    <body>
        $maybe msg <- mmsg
            <p .message>#{msg}
        ^{pageBody p}
|]

    -- | Override the rendering function for a particular URL. One use case for
    -- this is to offload static hosting to a different domain name to avoid
    -- sending cookies.
    urlRenderOverride :: a -> Route a -> Maybe S.ByteString
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

    -- | A function used to clean up path segments. It returns 'Nothing' when
    -- the given path is already clean, and a 'Just' when Yesod should redirect
    -- to the given path pieces.
    --
    -- * No double slashes
    --
    -- * There is no trailing slash.
    --
    -- Note that versions of Yesod prior to 0.7 used a different set of rules
    -- involing trailing slashes.
    cleanPath :: a -> [String] -> Maybe [String]
    cleanPath _ s =
        if corrected == s
            then Nothing
            else Just corrected
      where
        corrected = filter (not . null) s

    -- | Join the pieces of a path together into an absolute URL. This should
    -- be the inverse of 'splitPath'.
    --
    -- FIXME is this the right type sig?
    joinPath :: a
              -> S.ByteString -- ^ application root
              -> [String] -- ^ path pieces
              -> [(String, String)] -- ^ query string
              -> S.ByteString
    joinPath _ ar pieces qs =
        S.concat
            [ ar
            , S8.singleton '/'
            , S8.pack $ encodePathInfo pieces qs
            ]

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

    -- | Whether or not to tie a session to a specific IP address. Defaults to
    -- 'True'.
    sessionIpAddress :: a -> Bool
    sessionIpAddress _ = True

    yesodRunner :: YesodSite a => a -> Maybe CS.Key -> Maybe (Route a) -> GHandler a a ChooseRep -> W.Application
    yesodRunner = defaultYesodRunner

defaultYesodRunner :: (Yesod a, YesodSite a)
                   => a
                   -> Maybe CS.Key
                   -> Maybe (Route a)
                   -> GHandler a a ChooseRep
                   -> W.Application
defaultYesodRunner y mkey murl handler req = do
    now <- liftIO getCurrentTime
    let getExpires m = fromIntegral (m * 60) `addUTCTime` now
    let exp' = getExpires $ clientSessionDuration y
    -- FIXME will show remoteHost give the answer I need? will it include port
    -- information that changes on each request?
    let host = if sessionIpAddress y then S8.pack (show (W.remoteHost req)) else ""
    let session' =
            case mkey of
                Nothing -> []
                Just key -> fromMaybe [] $ do
                    raw <- lookup "Cookie" $ W.requestHeaders req
                    val <- lookup sessionName $ parseCookies raw
                    decodeSession key now host val
    rr <- liftIO $ parseWaiRequest req session' mkey
    let h = do
          case murl of
            Nothing -> handler
            Just url -> do
                isWrite <- isWriteRequest url
                ar <- isAuthorized url isWrite
                case ar of
                    Authorized -> return ()
                    AuthenticationRequired ->
                        case authRoute y of
                            Nothing ->
                                permissionDenied "Authentication required"
                            Just url' -> do
                                setUltDest'
                                redirect RedirectTemporary url'
                    Unauthorized s -> permissionDenied s
                handler
    let sessionMap = Map.fromList
                   $ filter (\(x, _) -> x /= nonceKey) session'
    yar <- handlerToYAR y (yesodRender y) errorHandler rr murl sessionMap h
    let mnonce = reqNonce rr
    return $ yarToResponse (hr mnonce getExpires host exp') yar
  where
    hr mnonce getExpires host exp' hs ct sm =
        hs'''
      where
        sessionVal =
            case (mkey, mnonce) of
                (Just key, Just nonce)
                    -> encodeSession key exp' host
                     $ Map.toList
                     $ Map.insert nonceKey nonce sm
                _ -> S.empty
        hs' =
            case mkey of
                Nothing -> hs
                Just _ -> AddCookie
                            (clientSessionDuration y)
                            sessionName
                            sessionVal
                          : hs
        hs'' = map (headerToPair getExpires) hs'
        hs''' = ("Content-Type", ct) : hs''

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

applyLayout' :: Yesod master
             => Html -- ^ title
             -> Hamlet (Route master) -- ^ body
             -> GHandler sub master ChooseRep
applyLayout' title body = fmap chooseRep $ defaultLayout $ do
    setTitle title
    addHamlet body

-- | The default error handler for 'errorHandler'.
defaultErrorHandler :: Yesod y => ErrorResponse -> GHandler sub y ChooseRep
defaultErrorHandler NotFound = do
    r <- waiRequest
    let path' = bsToChars $ W.pathInfo r
    applyLayout' "Not Found"
#if GHC7
        [hamlet|
#else
        [$hamlet|
#endif
<h1>Not Found
<p>#{path'}
|]
defaultErrorHandler (PermissionDenied msg) =
    applyLayout' "Permission Denied"
#if GHC7
        [hamlet|
#else
        [$hamlet|
#endif
<h1>Permission denied
<p>#{msg}
|]
defaultErrorHandler (InvalidArgs ia) =
    applyLayout' "Invalid Arguments"
#if GHC7
        [hamlet|
#else
        [$hamlet|
#endif
<h1>Invalid Arguments
<ul>
    $forall msg <- ia
        <li>#{msg}
|]
defaultErrorHandler (InternalError e) =
    applyLayout' "Internal Server Error"
#if GHC7
        [hamlet|
#else
        [$hamlet|
#endif
<h1>Internal Server Error
<p>#{e}
|]
defaultErrorHandler (BadMethod m) =
    applyLayout' "Bad Method"
#if GHC7
        [hamlet|
#else
        [$hamlet|
#endif
<h1>Method Not Supported
<p>Method "#{m}" not supported
|]

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
    let cssToHtml = preEscapedLazyText . renderCss
        celper :: Cassius url -> Hamlet url
        celper = fmap cssToHtml
        jsToHtml (Javascript b) = preEscapedLazyText $ toLazyText b
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
                   $ encodeUtf8 $ renderCassius render s
                return $ renderLoc x
    jsLoc <-
        case jscript of
            Nothing -> return Nothing
            Just s -> do
                x <- addStaticContent "js" "text/javascript; charset=utf-8"
                   $ encodeUtf8 $ renderJulius render s
                return $ renderLoc x

    let head'' =
#if GHC7
            [hamlet|
#else
            [$hamlet|
#endif
$forall s <- scripts
    <script src="^{s}">
$forall s <- stylesheets
    <link rel="stylesheet" href="^{s}">
$maybe s <- style
    $maybe s <- cssLoc
        <link rel="stylesheet" href="#{s}">
    $nothing
        <style>^{celper s}
$maybe j <- jscript
    $maybe s <- jsLoc
        <script src="#{s}">
    $nothing
        <script>^{jelper j}
\^{head'}
|]
    return $ PageContent title head'' body

yesodVersion :: String
yesodVersion = showVersion Paths_yesod_core.version

yesodRender :: (Yesod y, YesodSite y)
            => y
            -> Route y
            -> [(String, String)]
            -> String
yesodRender y u qs =
    S8.unpack $ fromMaybe
                (joinPath y (approot y) ps $ qs ++ qs')
                (urlRenderOverride y u)
  where
    (ps, qs') = formatPathSegments (getSite' y) u

#if TEST
coreTestSuite :: Test
coreTestSuite = testGroup "Yesod.Yesod"
    [ testProperty "join/split path" propJoinSplitPath
    , testCase "join/split path [\".\"]" caseJoinSplitPathDquote
    , testCase "utf8 split path" caseUtf8SplitPath
    , testCase "utf8 join path" caseUtf8JoinPath
    ]

data TmpYesod = TmpYesod
data TmpRoute = TmpRoute deriving Eq
type instance Route TmpYesod = TmpRoute
instance Yesod TmpYesod where approot _ = ""

fromString :: String -> S8.ByteString
fromString = Data.Text.Encoding.encodeUtf8 . Data.Text.pack

propJoinSplitPath :: [String] -> Bool
propJoinSplitPath ss =
    splitPath TmpYesod (fromString $ joinPath TmpYesod "" ss' [])
        == Right ss'
  where
    ss' = filter (not . null) ss

caseJoinSplitPathDquote :: Assertion
caseJoinSplitPathDquote = do
    splitPath TmpYesod (fromString "/x%2E/") @?= Right ["x."]
    splitPath TmpYesod (fromString "/y./") @?= Right ["y."]
    joinPath TmpYesod "" ["z."] [] @?= "/z./"
    x @?= Right ss
  where
    x = splitPath TmpYesod (fromString $ joinPath TmpYesod "" ss' [])
    ss' = filter (not . null) ss
    ss = ["a."]

caseUtf8SplitPath :: Assertion
caseUtf8SplitPath = do
    Right ["שלום"] @=?
        splitPath TmpYesod (fromString "/שלום/")
    Right ["page", "Fooé"] @=?
        splitPath TmpYesod (fromString "/page/Fooé/")
    Right ["\156"] @=?
        splitPath TmpYesod (fromString "/\156/")
    Right ["ð"] @=?
        splitPath TmpYesod (fromString "/%C3%B0/")

caseUtf8JoinPath :: Assertion
caseUtf8JoinPath = do
    "/%D7%A9%D7%9C%D7%95%D7%9D/" @=? joinPath TmpYesod "" ["שלום"] []
#endif
