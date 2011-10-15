{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | The basic typeclass for a Yesod application.
module Yesod.Internal.Core
    ( -- * Type classes
      Yesod (..)
    , YesodDispatch (..)
    , RenderRoute (..)
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
      -- * Logging
    , LogLevel (..)
    , formatLogMessage
    , fileLocationToString
    , messageLoggerHandler
      -- * Misc
    , yesodVersion
    , yesodRender
    ) where

import Yesod.Content
import Yesod.Handler

import Control.Arrow ((***))
import Control.Monad (forM)
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
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Control.Monad.Trans.RWS
import Text.Hamlet
import Text.Julius
import Text.Blaze ((!), customAttribute, textTag, toValue, unsafeLazyByteString)
import qualified Text.Blaze.Html5 as TBH
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Web.Cookie (parseCookies)
import qualified Data.Map as Map
import Data.Time
import Network.HTTP.Types (encodePath)
import qualified Data.Text as TS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Blaze.ByteString.Builder (Builder, toByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Data.List (foldl')
import qualified Network.HTTP.Types as H
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO
import qualified Data.Text.Lazy.Builder as TB
import Language.Haskell.TH.Syntax (Loc (..), Lift (..))
import Text.Blaze (preEscapedLazyText)
import Data.Aeson (Value (Array, String))
import Data.Aeson.Encode (encode)
import qualified Data.Vector as Vector

#if GHC7
#define HAMLET hamlet
#else
#define HAMLET $hamlet
#endif

class Eq u => RenderRoute u where
    renderRoute :: u -> ([Text], [(Text, Text)])

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function. You should never need to deal with it directly.
class YesodDispatch a master where
    yesodDispatch
        :: Yesod master
        => a
        -> Maybe CS.Key
        -> [Text]
        -> master
        -> (Route a -> Route master)
        -> Maybe W.Application

    yesodRunner :: Yesod master
                => a
                -> master
                -> (Route a -> Route master)
                -> Maybe CS.Key -> Maybe (Route a) -> GHandler a master ChooseRep -> W.Application
    yesodRunner = defaultYesodRunner

-- | Define settings for a Yesod applications. The only required setting is
-- 'approot'; other than that, there are intelligent defaults.
class RenderRoute (Route a) => Yesod a where
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
    approot :: a -> Text

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
    urlRenderOverride :: a -> Route a -> Maybe Builder
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
        return $ W.requestMethod wai `notElem`
            ["GET", "HEAD", "OPTIONS", "TRACE"]

    -- | The default route for authentication.
    --
    -- Used in particular by 'isAuthorized', but library users can do whatever
    -- they want with it.
    authRoute :: a -> Maybe (Route a)
    authRoute _ = Nothing

    -- | A function used to clean up path segments. It returns 'Right' with a
    -- clean path or 'Left' with a new set of pieces the user should be
    -- redirected to. The default implementation enforces:
    --
    -- * No double slashes
    --
    -- * There is no trailing slash.
    --
    -- Note that versions of Yesod prior to 0.7 used a different set of rules
    -- involing trailing slashes.
    cleanPath :: a -> [Text] -> Either [Text] [Text]
    cleanPath _ s =
        if corrected == s
            then Right s
            else Left corrected
      where
        corrected = filter (not . TS.null) s

    -- | Builds an absolute URL by concatenating the application root with the  
    -- pieces of a path and a query string, if any. 
    -- Note that the pieces of the path have been previously cleaned up by 'cleanPath'.
    joinPath :: a
             -> TS.Text -- ^ application root
             -> [TS.Text] -- ^ path pieces
             -> [(TS.Text, TS.Text)] -- ^ query string
             -> Builder
    joinPath _ ar pieces' qs' = fromText ar `mappend` encodePath pieces qs
      where
        pieces = if null pieces' then [""] else pieces'
        qs = map (TE.encodeUtf8 *** go) qs'
        go "" = Nothing
        go x = Just $ TE.encodeUtf8 x

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
    addStaticContent :: Text -- ^ filename extension
                     -> Text -- ^ mime-type
                     -> L.ByteString -- ^ content
                     -> GHandler sub a (Maybe (Either Text (Route a, [(Text, Text)])))
    addStaticContent _ _ _ = return Nothing

    -- | Whether or not to tie a session to a specific IP address. Defaults to
    -- 'True'.
    sessionIpAddress :: a -> Bool
    sessionIpAddress _ = True

    -- | The path value to set for cookies. By default, uses \"\/\", meaning
    -- cookies will be sent to every page on the current domain.
    cookiePath :: a -> S8.ByteString
    cookiePath _ = "/"

    -- | Maximum allowed length of the request body, in bytes.
    maximumContentLength :: a -> Maybe (Route a) -> Int
    maximumContentLength _ _ = 2 * 1024 * 1024 -- 2 megabytes

    -- | Send a message to the log. By default, prints to stderr.
    messageLogger :: a
                  -> Loc -- ^ position in source code
                  -> LogLevel
                  -> Text -- ^ message
                  -> IO ()
    messageLogger _ loc level msg =
        formatLogMessage loc level msg >>=
        Data.Text.Lazy.IO.putStrLn

    -- | Apply gzip compression to files. Default is false.
    gzipCompressFiles :: a -> Bool
    gzipCompressFiles _ = False

    -- | Location of yepnope.js, if any. If one is provided, then all
    -- Javascript files will be loaded asynchronously.
    yepnopeJs :: a -> Maybe (Either Text (Route a))
    yepnopeJs _ = Nothing

messageLoggerHandler :: (Yesod m, MonadIO mo)
                     => Loc -> LogLevel -> Text -> GGHandler s m mo ()
messageLoggerHandler loc level msg = do
    y <- getYesod
    liftIO $ messageLogger y loc level msg

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther Text
    deriving (Eq, Show, Read, Ord)

instance Lift LogLevel where
    lift LevelDebug = [|LevelDebug|]
    lift LevelInfo = [|LevelInfo|]
    lift LevelWarn = [|LevelWarn|]
    lift LevelError = [|LevelError|]
    lift (LevelOther x) = [|LevelOther $ TS.pack $(lift $ TS.unpack x)|]

formatLogMessage :: Loc
                 -> LogLevel
                 -> Text -- ^ message
                 -> IO TL.Text
formatLogMessage loc level msg = do
    now <- getCurrentTime
    return $ TB.toLazyText $
        TB.fromText (TS.pack $ show now)
        `mappend` TB.fromText " ["
        `mappend` TB.fromText (TS.pack $ drop 5 $ show level)
        `mappend` TB.fromText "] "
        `mappend` TB.fromText msg
        `mappend` TB.fromText " @("
        `mappend` TB.fromText (TS.pack $ fileLocationToString loc)
        `mappend` TB.fromText ") "

-- taken from file-location package
-- turn the TH Loc loaction information into a human readable string
-- leaving out the loc_end parameter
fileLocationToString :: Loc -> String
fileLocationToString loc = (loc_package loc) ++ ':' : (loc_module loc) ++  
  ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
  where
    line = show . fst . loc_start
    char = show . snd . loc_start

defaultYesodRunner :: Yesod master
                   => a
                   -> master
                   -> (Route a -> Route master)
                   -> Maybe CS.Key
                   -> Maybe (Route a)
                   -> GHandler a master ChooseRep
                   -> W.Application
defaultYesodRunner _ m toMaster _ murl _ req
    | maximumContentLength m (fmap toMaster murl) < len =
        return $ W.responseLBS
            (H.Status 413 "Too Large")
            [("Content-Type", "text/plain")]
            "Request body too large to be processed."
  where
    len = fromMaybe 0 $ lookup "content-length" (W.requestHeaders req) >>= readMay
    readMay s =
        case reads $ S8.unpack s of
            [] -> Nothing
            (x, _):_ -> Just x
defaultYesodRunner s master toMasterRoute mkey murl handler req = do
    now <- liftIO getCurrentTime
    let getExpires m = fromIntegral (m * 60) `addUTCTime` now
    let exp' = getExpires $ clientSessionDuration master
    let rh = takeWhile (/= ':') $ show $ W.remoteHost req
    let host = if sessionIpAddress master then S8.pack rh else ""
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
                isWrite <- isWriteRequest $ toMasterRoute url
                ar <- isAuthorized (toMasterRoute url) isWrite
                case ar of
                    Authorized -> return ()
                    AuthenticationRequired ->
                        case authRoute master of
                            Nothing ->
                                permissionDenied "Authentication required"
                            Just url' -> do
                                setUltDest'
                                redirect RedirectTemporary url'
                    Unauthorized s' -> permissionDenied s'
                handler
    let sessionMap = Map.fromList
                   $ filter (\(x, _) -> x /= nonceKey) session'
    yar <- handlerToYAR master s toMasterRoute (yesodRender master) errorHandler rr murl sessionMap h
    let mnonce = reqNonce rr
    iv <- liftIO CS.randomIV
    return $ yarToResponse (hr iv mnonce getExpires host exp') yar
  where
    hr iv mnonce getExpires host exp' hs ct sm =
        hs'''
      where
        sessionVal =
            case (mkey, mnonce) of
                (Just key, Just nonce)
                    -> encodeSession key iv exp' host
                     $ Map.toList
                     $ Map.insert nonceKey nonce sm
                _ -> mempty
        hs' =
            case mkey of
                Nothing -> hs
                Just _ -> AddCookie
                            (clientSessionDuration master)
                            sessionName
                            sessionVal
                          : hs
        hs'' = map (headerToPair (cookiePath master) getExpires) hs'
        hs''' = ("Content-Type", ct) : hs''

data AuthResult = Authorized | AuthenticationRequired | Unauthorized Text
    deriving (Eq, Show, Read)

-- | A type-safe, concise method of creating breadcrumbs for pages. For each
-- resource, you declare the title of the page and the parent resource (if
-- present).
class YesodBreadcrumbs y where
    -- | Returns the title and the parent resource, if available. If you return
    -- a 'Nothing', then this is considered a top-level page.
    breadcrumb :: Route y -> GHandler sub y (Text , Maybe (Route y))

-- | Gets the title of the current page and the hierarchy of parent pages,
-- along with their respective titles.
breadcrumbs :: YesodBreadcrumbs y => GHandler sub y (Text, [(Route y, Text)])
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
             -> HtmlUrl (Route master) -- ^ body
             -> GHandler sub master ChooseRep
applyLayout' title body = fmap chooseRep $ defaultLayout $ do
    setTitle title
    addHamlet body

-- | The default error handler for 'errorHandler'.
defaultErrorHandler :: Yesod y => ErrorResponse -> GHandler sub y ChooseRep
defaultErrorHandler NotFound = do
    r <- waiRequest
    let path' = TE.decodeUtf8With TEE.lenientDecode $ W.rawPathInfo r
    applyLayout' "Not Found"
        [HAMLET|
<h1>Not Found
<p>#{path'}
|]
defaultErrorHandler (PermissionDenied msg) =
    applyLayout' "Permission Denied"
        [HAMLET|
<h1>Permission denied
<p>#{msg}
|]
defaultErrorHandler (InvalidArgs ia) =
    applyLayout' "Invalid Arguments"
        [HAMLET|
<h1>Invalid Arguments
<ul>
    $forall msg <- ia
        <li>#{msg}
|]
defaultErrorHandler (InternalError e) =
    applyLayout' "Internal Server Error"
        [HAMLET|
<h1>Internal Server Error
<p>#{e}
|]
defaultErrorHandler (BadMethod m) =
    applyLayout' "Bad Method"
        [HAMLET|
<h1>Method Not Supported
<p>Method "#{S8.unpack m}" not supported
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

jsToHtml :: Javascript -> Html
jsToHtml (Javascript b) = preEscapedLazyText $ toLazyText b

jelper :: JavascriptUrl url -> HtmlUrl url
jelper = fmap jsToHtml

-- | Convert a widget to a 'PageContent'.
widgetToPageContent :: (Eq (Route master), Yesod master)
                    => GWidget sub master ()
                    -> GHandler sub master (PageContent (Route master))
widgetToPageContent (GWidget w) = do
    master <- getYesod
    ((), _, GWData (Body body) (Last mTitle) scripts' stylesheets' style jscript (Head head')) <- runRWST w () 0
    let title = maybe mempty unTitle mTitle
    let scripts = runUniqueList scripts'
    let stylesheets = runUniqueList stylesheets'

    render <- getUrlRenderParams
    let renderLoc x =
            case x of
                Nothing -> Nothing
                Just (Left s) -> Just s
                Just (Right (u, p)) -> Just $ render u p
    css <- forM (Map.toList style) $ \(mmedia, content) -> do
        let rendered = toLazyText $ content render
        x <- addStaticContent "css" "text/css; charset=utf-8"
           $ encodeUtf8 rendered
        return (mmedia,
            case x of
                Nothing -> Left $ preEscapedLazyText rendered
                Just y -> Right $ either id (uncurry render) y)
    jsLoc <-
        case jscript of
            Nothing -> return Nothing
            Just s -> do
                x <- addStaticContent "js" "text/javascript; charset=utf-8"
                   $ encodeUtf8 $ renderJavascriptUrl render s
                return $ renderLoc x

    let addAttr x (y, z) = x ! customAttribute (textTag y) (toValue z)
    let renderLoc' render' (Local url) = render' url []
        renderLoc' _ (Remote s) = s
    let mkScriptTag (Script loc attrs) render' =
            foldl' addAttr TBH.script (("src", renderLoc' render' loc) : attrs) $ return ()
    let mkLinkTag (Stylesheet loc attrs) render' =
            foldl' addAttr TBH.link
                ( ("rel", "stylesheet")
                : ("href", renderLoc' render' loc)
                : attrs
                )
    let left (Left x) = Just x
        left _ = Nothing
        right (Right x) = Just x
        right _ = Nothing
    let head'' = [HAMLET|
$forall s <- stylesheets
    ^{mkLinkTag s}
$forall s <- css
    $maybe t <- right $ snd s
        $maybe media <- fst s
            <link rel=stylesheet media=#{media} href=#{t}
        $nothing
            <link rel=stylesheet href=#{t}
    $maybe content <- left $ snd s
        $maybe media <- fst s
            <style media=#{media}>#{content}
        $nothing
            <style>#{content}
$maybe _ <- yepnopeJs master
$nothing
    $forall s <- scripts
        ^{mkScriptTag s}
    $maybe j <- jscript
        $maybe s <- jsLoc
            <script src="#{s}">
        $nothing
            <script>^{jelper j}
\^{head'}
|]
    let (mcomplete, ynscripts) = ynHelper render scripts jscript jsLoc
    let bodyYN = [HAMLET|
^{body}
$maybe eyn <- yepnopeJs master
    $maybe yn <- left eyn
        <script src=#{yn}>
    $maybe yn <- right eyn
        <script src=@{yn}>
    $maybe complete <- mcomplete
        <script>yepnope({load:#{ynscripts},complete:function(){^{complete}}})
    $nothing
        <script>yepnope({load:#{ynscripts}})
|]
    return $ PageContent title head'' bodyYN

ynHelper :: (url -> [x] -> Text)
         -> [Script (url)]
         -> Maybe (JavascriptUrl (url))
         -> Maybe Text
         -> (Maybe (HtmlUrl (url)), Html)
ynHelper render scripts jscript jsLoc =
    (mcomplete, unsafeLazyByteString $ encode $ Array $ Vector.fromList $ map String scripts'')
  where
    scripts' = map goScript scripts
    scripts'' =
        case jsLoc of
            Just s -> scripts' ++ [s]
            Nothing -> scripts'
    goScript (Script (Local url) _) = render url []
    goScript (Script (Remote s) _) = s
    mcomplete =
        case jsLoc of
            Just{} -> Nothing
            Nothing ->
                case jscript of
                    Nothing -> Nothing
                    Just j -> Just $ jelper j

yesodVersion :: String
yesodVersion = showVersion Paths_yesod_core.version

yesodRender :: Yesod y
            => y
            -> Route y
            -> [(Text, Text)]
            -> Text
yesodRender y u qs =
    TE.decodeUtf8 $ toByteString $
    fromMaybe
        (joinPath y (approot y) ps
          $ qs ++ qs')
        (urlRenderOverride y u)
  where
    (ps, qs') = renderRoute u
