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
      -- * Sessions
    , SessionBackend (..)
    , defaultClientSessionBackend
    , clientSessionBackend
    , loadClientSession
    , BackendSession
      -- * jsLoader
    , ScriptLoadPosition (..)
    , BottomOfHeadAsync
    , loadJsYepnope
      -- * Misc
    , yesodVersion
    , yesodRender
    , resolveApproot
    , Approot (..)
    , FileUpload (..)
    , runFakeHandler
    ) where

import Yesod.Content
import Yesod.Handler hiding (lift, getExpires)

import Yesod.Routes.Class

import Data.Word (Word64)
import Control.Arrow ((***))
import Control.Monad (forM)
import Yesod.Widget
import Yesod.Request
import qualified Network.Wai as W
import Yesod.Internal
import Yesod.Internal.Session
import Yesod.Internal.Request
import qualified Web.ClientSession as CS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.IORef as I
import Data.Monoid
import Text.Hamlet
import Text.Julius
import Text.Blaze ((!), customAttribute, textTag, toValue, unsafeLazyByteString)
import qualified Text.Blaze.Html5 as TBH
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe, isJust)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (runResourceT)
import Web.Cookie (parseCookies)
import qualified Data.Map as Map
import Data.Time
import Network.HTTP.Types (encodePath)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Blaze.ByteString.Builder (Builder, toByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Data.List (foldl')
import qualified Network.HTTP.Types as H
import Web.Cookie (SetCookie (..))
import Language.Haskell.TH.Syntax (Loc (..))
import Text.Blaze (preEscapedToMarkup)
import Data.Aeson (Value (Array, String))
import Data.Aeson.Encode (encode)
import qualified Data.Vector as Vector
import Network.Wai.Middleware.Gzip (GzipSettings, def)
import Network.Wai.Parse (tempFileBackEnd, lbsBackEnd)
import qualified Paths_yesod_core
import Data.Version (showVersion)
import System.Log.FastLogger (Logger, mkLogger, loggerDate, LogStr (..), loggerPutStr)
import Control.Monad.Logger (LogLevel (LevelInfo, LevelOther), LogSource)
import System.Log.FastLogger.Date (ZonedDate)
import System.IO (stdout)

yesodVersion :: String
yesodVersion = showVersion Paths_yesod_core.version

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function. You should never need to deal with it directly.
class YesodDispatch sub master where
    yesodDispatch
        :: Yesod master
        => Logger
        -> master
        -> sub
        -> (Route sub -> Route master)
        -> (Maybe (SessionBackend master) -> W.Application) -- ^ 404 handler
        -> (Route sub -> Maybe (SessionBackend master) -> W.Application) -- ^ 405 handler
        -> Text -- ^ request method
        -> [Text] -- ^ pieces
        -> Maybe (SessionBackend master)
        -> W.Application

    yesodRunner :: Yesod master
                => Logger
                -> GHandler sub master ChooseRep
                -> master
                -> sub
                -> Maybe (Route sub)
                -> (Route sub -> Route master)
                -> Maybe (SessionBackend master)
                -> W.Application
    yesodRunner = defaultYesodRunner

-- | How to determine the root of the application for constructing URLs.
--
-- Note that future versions of Yesod may add new constructors without bumping
-- the major version number. As a result, you should /not/ pattern match on
-- @Approot@ values.
data Approot master = ApprootRelative -- ^ No application root.
                    | ApprootStatic Text
                    | ApprootMaster (master -> Text)
                    | ApprootRequest (master -> W.Request -> Text)

type ResolvedApproot = Text

-- | Define settings for a Yesod applications. All methods have intelligent
-- defaults, and therefore no implementation is required.
class RenderRoute a => Yesod a where
    -- | An absolute URL to the root of the application. Do not include
    -- trailing slash.
    --
    -- Default value: 'ApprootRelative'. This is valid under the following
    -- conditions:
    --
    -- * Your application is served from the root of the domain.
    --
    -- * You do not use any features that require absolute URLs, such as Atom
    -- feeds and XML sitemaps.
    --
    -- If this is not true, you should override with a different
    -- implementation.
    approot :: Approot a
    approot = ApprootRelative

    -- | Output error response pages.
    errorHandler :: ErrorResponse -> GHandler sub a ChooseRep
    errorHandler = defaultErrorHandler

    -- | Applies some form of layout to the contents of a page.
    defaultLayout :: GWidget sub a () -> GHandler sub a RepHtml
    defaultLayout w = do
        p <- widgetToPageContent w
        mmsg <- getMessage
        hamletToRepHtml [hamlet|
$newline never
$doctype 5

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
    -- Return 'Authorized' if the request is authorized,
    -- 'Unauthorized' a message if unauthorized.
    -- If authentication is required, return 'AuthenticationRequired'.
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
            then Right $ map dropDash s
            else Left corrected
      where
        corrected = filter (not . T.null) s
        dropDash t
            | T.all (== '-') t = T.drop 1 t
            | otherwise = t

    -- | Builds an absolute URL by concatenating the application root with the
    -- pieces of a path and a query string, if any.
    -- Note that the pieces of the path have been previously cleaned up by 'cleanPath'.
    joinPath :: a
             -> T.Text -- ^ application root
             -> [T.Text] -- ^ path pieces
             -> [(T.Text, T.Text)] -- ^ query string
             -> Builder
    joinPath _ ar pieces' qs' =
        fromText ar `mappend` encodePath pieces qs
      where
        pieces = if null pieces' then [""] else map addDash pieces'
        qs = map (TE.encodeUtf8 *** go) qs'
        go "" = Nothing
        go x = Just $ TE.encodeUtf8 x
        addDash t
            | T.all (== '-') t = T.cons '-' t
            | otherwise = t

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

    {- Temporarily disabled until we have a better interface.
    -- | Whether or not to tie a session to a specific IP address. Defaults to
    -- 'False'.
    --
    -- Note: This setting has two known problems: it does not work correctly
    -- when behind a reverse proxy (including load balancers), and it may not
    -- function correctly if the user is behind a proxy.
    sessionIpAddress :: a -> Bool
    sessionIpAddress _ = False
    -}

    -- | The path value to set for cookies. By default, uses \"\/\", meaning
    -- cookies will be sent to every page on the current domain.
    cookiePath :: a -> S8.ByteString
    cookiePath _ = "/"

    -- | The domain value to set for cookies. By default, the
    -- domain is not set, meaning cookies will be sent only to
    -- the current domain.
    cookieDomain :: a -> Maybe S8.ByteString
    cookieDomain _ = Nothing

    -- | Maximum allowed length of the request body, in bytes.
    --
    -- Default: 2 megabytes.
    maximumContentLength :: a -> Maybe (Route a) -> Word64
    maximumContentLength _ _ = 2 * 1024 * 1024 -- 2 megabytes

    -- | Returns a @Logger@ to use for log messages.
    --
    -- Default: Sends to stdout and automatically flushes on each write.
    getLogger :: a -> IO Logger
    getLogger _ = mkLogger True stdout

    -- | Send a message to the @Logger@ provided by @getLogger@.
    --
    -- Note: This method is no longer used. Instead, you should override
    -- 'messageLoggerSource'.
    messageLogger :: a
                  -> Logger
                  -> Loc -- ^ position in source code
                  -> LogLevel
                  -> LogStr -- ^ message
                  -> IO ()
    messageLogger a logger loc = messageLoggerSource a logger loc ""

    -- | Send a message to the @Logger@ provided by @getLogger@.
    messageLoggerSource :: a
                        -> Logger
                        -> Loc -- ^ position in source code
                        -> LogSource
                        -> LogLevel
                        -> LogStr -- ^ message
                        -> IO ()
    messageLoggerSource a logger loc source level msg =
        if shouldLog a source level
            then formatLogMessage (loggerDate logger) loc source level msg >>= loggerPutStr logger
            else return ()

    -- | The logging level in place for this application. Any messages below
    -- this level will simply be ignored.
    logLevel :: a -> LogLevel
    logLevel _ = LevelInfo

    -- | GZIP settings.
    gzipSettings :: a -> GzipSettings
    gzipSettings _ = def

    -- | Where to Load sripts from. We recommend the default value,
    -- 'BottomOfBody'.  Alternatively use the built in async yepnope loader:
    --
    -- > BottomOfHeadAsync $ loadJsYepnope $ Right $ StaticR js_modernizr_js
    --
    -- Or write your own async js loader: see 'loadJsYepnope'
    jsLoader :: a -> ScriptLoadPosition a
    jsLoader _ = BottomOfBody

    -- | Create a session backend. Returning `Nothing' disables sessions.
    --
    -- Default: Uses clientsession with a 2 hour timeout.
    makeSessionBackend :: a -> IO (Maybe (SessionBackend a))
    makeSessionBackend _ = do
        key <- CS.getKey CS.defaultKeyFile
        return $ Just $ clientSessionBackend key 120

    -- | How to store uploaded files.
    --
    -- Default: Whe nthe request body is greater than 50kb, store in a temp
    -- file. Otherwise, store in memory.
    fileUpload :: a
               -> Word64 -- ^ request body size
               -> FileUpload
    fileUpload _ size
        | size > 50000 = FileUploadDisk tempFileBackEnd
        | otherwise = FileUploadMemory lbsBackEnd

    -- | Should we log the given log source/level combination.
    --
    -- Default: Logs everything at or above 'logLevel'
    shouldLog :: a -> LogSource -> LogLevel -> Bool
    shouldLog a _ level = level >= logLevel a

{-# DEPRECATED messageLogger "Please use messageLoggerSource (since yesod-core 1.1.2)" #-}

formatLogMessage :: IO ZonedDate
                 -> Loc
                 -> LogSource
                 -> LogLevel
                 -> LogStr -- ^ message
                 -> IO [LogStr]
formatLogMessage getdate loc src level msg = do
    now <- getdate
    return
        [ LB now
        , LB " ["
        , LS $
            case level of
                LevelOther t -> T.unpack t
                _ -> drop 5 $ show level
        , LS $
            if T.null src
                then ""
                else "#" ++ T.unpack src
        , LB "] "
        , msg
        , LB " @("
        , LS $ fileLocationToString loc
        , LB ")\n"
        ]

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
                   => Logger
                   -> GHandler sub master ChooseRep
                   -> master
                   -> sub
                   -> Maybe (Route sub)
                   -> (Route sub -> Route master)
                   -> Maybe (SessionBackend master)
                   -> W.Application
defaultYesodRunner logger handler master sub murl toMasterRoute msb req
  | maximumContentLength master (fmap toMasterRoute murl) < len =
        return $ W.responseLBS
            (H.Status 413 "Too Large")
            [("Content-Type", "text/plain")]
            "Request body too large to be processed."
  | otherwise = do
    now <- liftIO getCurrentTime
    let dontSaveSession _ _ = return []
    (session, saveSession) <- liftIO $
        maybe (return ([], dontSaveSession)) (\sb -> sbLoadSession sb master req now) msb
    rr <- liftIO $ parseWaiRequest req session (isJust msb) len
    let h = {-# SCC "h" #-} do
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
                                setUltDestCurrent
                                redirect url'
                    Unauthorized s' -> permissionDenied s'
                handler
    let sessionMap = Map.fromList . filter ((/=) tokenKey . fst) $ session
    let ra = resolveApproot master req
    let log' = messageLoggerSource master logger
    yar <- handlerToYAR master sub (fileUpload master) log' toMasterRoute
        (yesodRender master ra) errorHandler rr murl sessionMap h
    extraHeaders <- case yar of
        (YARPlain _ _ ct _ newSess) -> do
            let nsToken = Map.toList $ maybe
                    newSess
                    (\n -> Map.insert tokenKey (TE.encodeUtf8 n) newSess)
                    (reqToken rr)
            sessionHeaders <- liftIO (saveSession nsToken now)
            return $ ("Content-Type", ct) : map headerToPair sessionHeaders
        _ -> return []
    return $ yarToResponse yar extraHeaders
  where
    len = fromMaybe 0 $ lookup "content-length" (W.requestHeaders req) >>= readMay
    readMay s =
        case reads $ S8.unpack s of
            [] -> Nothing
            (x, _):_ -> Just x

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
    toWidget body

-- | The default error handler for 'errorHandler'.
defaultErrorHandler :: Yesod y => ErrorResponse -> GHandler sub y ChooseRep
defaultErrorHandler NotFound = do
    r <- waiRequest
    let path' = TE.decodeUtf8With TEE.lenientDecode $ W.rawPathInfo r
    applyLayout' "Not Found"
        [hamlet|
$newline never
<h1>Not Found
<p>#{path'}
|]
defaultErrorHandler (PermissionDenied msg) =
    applyLayout' "Permission Denied"
        [hamlet|
$newline never
<h1>Permission denied
<p>#{msg}
|]
defaultErrorHandler (InvalidArgs ia) =
    applyLayout' "Invalid Arguments"
        [hamlet|
$newline never
<h1>Invalid Arguments
<ul>
    $forall msg <- ia
        <li>#{msg}
|]
defaultErrorHandler (InternalError e) =
    applyLayout' "Internal Server Error"
        [hamlet|
$newline never
<h1>Internal Server Error
<p>#{e}
|]
defaultErrorHandler (BadMethod m) =
    applyLayout' "Bad Method"
        [hamlet|
$newline never
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
jsToHtml (Javascript b) = preEscapedToMarkup $ toLazyText b

jelper :: JavascriptUrl url -> HtmlUrl url
jelper = fmap jsToHtml

-- | Convert a widget to a 'PageContent'.
widgetToPageContent :: (Eq (Route master), Yesod master)
                    => GWidget sub master ()
                    -> GHandler sub master (PageContent (Route master))
widgetToPageContent w = do
    master <- getYesod
    ((), GWData (Body body) (Last mTitle) scripts' stylesheets' style jscript (Head head')) <- unGWidget w
    let title = maybe mempty unTitle mTitle
        scripts = runUniqueList scripts'
        stylesheets = runUniqueList stylesheets'

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
                Nothing -> Left $ preEscapedToMarkup rendered
                Just y -> Right $ either id (uncurry render) y)
    jsLoc <-
        case jscript of
            Nothing -> return Nothing
            Just s -> do
                x <- addStaticContent "js" "text/javascript; charset=utf-8"
                   $ encodeUtf8 $ renderJavascriptUrl render s
                return $ renderLoc x

    -- modernizr should be at the end of the <head> http://www.modernizr.com/docs/#installing
    -- the asynchronous loader means your page doesn't have to wait for all the js to load
    let (mcomplete, asyncScripts) = asyncHelper render scripts jscript jsLoc
        regularScriptLoad = [hamlet|
$newline never
$forall s <- scripts
    ^{mkScriptTag s}
$maybe j <- jscript
    $maybe s <- jsLoc
        <script src="#{s}">
    $nothing
        <script>^{jelper j}
|]

        headAll = [hamlet|
$newline never
\^{head'}
$forall s <- stylesheets
    ^{mkLinkTag s}
$forall s <- css
    $maybe t <- right $ snd s
        $maybe media <- fst s
            <link rel=stylesheet media=#{media} href=#{t}>
        $nothing
            <link rel=stylesheet href=#{t}>
    $maybe content <- left $ snd s
        $maybe media <- fst s
            <style media=#{media}>#{content}
        $nothing
            <style>#{content}
$case jsLoader master
  $of BottomOfBody
  $of BottomOfHeadAsync asyncJsLoader
      ^{asyncJsLoader asyncScripts mcomplete}
  $of BottomOfHeadBlocking
      ^{regularScriptLoad}
|]
    let bodyScript = [hamlet|
$newline never
^{body}
^{regularScriptLoad}
|]

    return $ PageContent title headAll (case jsLoader master of
      BottomOfBody -> bodyScript
      _ -> body)
  where
    renderLoc' render' (Local url) = render' url []
    renderLoc' _ (Remote s) = s

    addAttr x (y, z) = x ! customAttribute (textTag y) (toValue z)
    mkScriptTag (Script loc attrs) render' =
        foldl' addAttr TBH.script (("src", renderLoc' render' loc) : attrs) $ return ()
    mkLinkTag (Stylesheet loc attrs) render' =
        foldl' addAttr TBH.link
            ( ("rel", "stylesheet")
            : ("href", renderLoc' render' loc)
            : attrs
            )

data ScriptLoadPosition master
    = BottomOfBody
    | BottomOfHeadBlocking
    | BottomOfHeadAsync (BottomOfHeadAsync master)

type BottomOfHeadAsync master
       = [Text] -- ^ urls to load asynchronously
      -> Maybe (HtmlUrl (Route master)) -- ^ widget of js to run on async completion
      -> (HtmlUrl (Route master)) -- ^ widget to insert at the bottom of <head>

left :: Either a b -> Maybe a
left (Left x) = Just x
left _ = Nothing

right :: Either a b -> Maybe b
right (Right x) = Just x
right _ = Nothing

jsonArray :: [Text] -> Html
jsonArray = unsafeLazyByteString . encode . Array . Vector.fromList . map String

-- | For use with setting 'jsLoader' to 'BottomOfHeadAsync'
loadJsYepnope :: Yesod master => Either Text (Route master) -> [Text] -> Maybe (HtmlUrl (Route master)) -> (HtmlUrl (Route master))
loadJsYepnope eyn scripts mcomplete =
  [hamlet|
$newline never
    $maybe yn <- left eyn
        <script src=#{yn}>
    $maybe yn <- right eyn
        <script src=@{yn}>
    $maybe complete <- mcomplete
        <script>yepnope({load:#{jsonArray scripts},complete:function(){^{complete}}});
    $nothing
        <script>yepnope({load:#{jsonArray scripts}});
|]

asyncHelper :: (url -> [x] -> Text)
         -> [Script (url)]
         -> Maybe (JavascriptUrl (url))
         -> Maybe Text
         -> (Maybe (HtmlUrl url), [Text])
asyncHelper render scripts jscript jsLoc =
    (mcomplete, scripts'')
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

yesodRender :: Yesod y
            => y
            -> ResolvedApproot
            -> Route y
            -> [(Text, Text)] -- ^ url query string
            -> Text
yesodRender y ar url params =
    TE.decodeUtf8 $ toByteString $
    fromMaybe
        (joinPath y ar ps
          $ params ++ params')
        (urlRenderOverride y url)
  where
    (ps, params') = renderRoute url

resolveApproot :: Yesod master => master -> W.Request -> ResolvedApproot
resolveApproot master req =
    case approot of
        ApprootRelative -> ""
        ApprootStatic t -> t
        ApprootMaster f -> f master
        ApprootRequest f -> f master req

defaultClientSessionBackend :: Yesod master => IO (SessionBackend master)
defaultClientSessionBackend = do
  key <- CS.getKey CS.defaultKeyFile
  let timeout = 120 -- 120 minutes
  return $ clientSessionBackend key timeout

clientSessionBackend :: Yesod master
                     => CS.Key  -- ^ The encryption key
                     -> Int -- ^ Inactive session valitity in minutes
                     -> SessionBackend master
clientSessionBackend key timeout = SessionBackend
    { sbLoadSession = loadClientSession key timeout "_SESSION"
    }

loadClientSession :: Yesod master
                  => CS.Key
                  -> Int -- ^ timeout
                  -> S8.ByteString -- ^ session name
                  -> master
                  -> W.Request
                  -> UTCTime
                  -> IO (BackendSession, SaveSession)
loadClientSession key timeout sessionName master req now = return (sess, save)
  where
    sess = fromMaybe [] $ do
      raw <- lookup "Cookie" $ W.requestHeaders req
      val <- lookup sessionName $ parseCookies raw
      let host = "" -- fixme, properly lock sessions to client address
      decodeClientSession key now host val
    save sess' now' = do
      -- We should never cache the IV!  Be careful!
      iv <- liftIO CS.randomIV
      return [AddCookie def
          { setCookieName = sessionName
          , setCookieValue = sessionVal iv
          , setCookiePath = Just (cookiePath master)
          , setCookieExpires = Just expires
          , setCookieDomain = cookieDomain master
          , setCookieHttpOnly = True
          }]
        where
          host = "" -- fixme, properly lock sessions to client address
          expires = fromIntegral (timeout * 60) `addUTCTime` now'
          sessionVal iv = encodeClientSession key iv expires host sess'


-- | Run a 'GHandler' completely outside of Yesod.  This
-- function comes with many caveats and you shouldn't use it
-- unless you fully understand what it's doing and how it works.
--
-- As of now, there's only one reason to use this function at
-- all: in order to run unit tests of functions inside 'GHandler'
-- but that aren't easily testable with a full HTTP request.
-- Even so, it's better to use @wai-test@ or @yesod-test@ instead
-- of using this function.
--
-- This function will create a fake HTTP request (both @wai@'s
-- 'W.Request' and @yesod@'s 'Request') and feed it to the
-- @GHandler@.  The only useful information the @GHandler@ may
-- get from the request is the session map, which you must supply
-- as argument to @runFakeHandler@.  All other fields contain
-- fake information, which means that they can be accessed but
-- won't have any useful information.  The response of the
-- @GHandler@ is completely ignored, including changes to the
-- session, cookies or headers.  We only return you the
-- @GHandler@'s return value.
runFakeHandler :: (Yesod master, MonadIO m) =>
                  SessionMap
               -> (master -> Logger)
               -> master
               -> GHandler master master a
               -> m (Either ErrorResponse a)
runFakeHandler fakeSessionMap logger master handler = liftIO $ do
  ret <- I.newIORef (Left $ InternalError "runFakeHandler: no result")
  let handler' = do liftIO . I.writeIORef ret . Right =<< handler
                    return ()
  let YesodApp yapp =
        runHandler
          handler'
          (yesodRender master "")
          Nothing
          id
          master
          master
          (fileUpload master)
          (messageLoggerSource master $ logger master)
      errHandler err =
        YesodApp $ \_ _ _ session -> do
          liftIO $ I.writeIORef ret (Left err)
          return $ YARPlain
                     H.status500
                     []
                     typePlain
                     (toContent ("runFakeHandler: errHandler" :: S8.ByteString))
                     session
      fakeWaiRequest =
        W.Request
          { W.requestMethod  = "POST"
          , W.httpVersion    = H.http11
          , W.rawPathInfo    = "/runFakeHandler/pathInfo"
          , W.rawQueryString = ""
          , W.serverName     = "runFakeHandler-serverName"
          , W.serverPort     = 80
          , W.requestHeaders = []
          , W.isSecure       = False
          , W.remoteHost     = error "runFakeHandler-remoteHost"
          , W.pathInfo       = ["runFakeHandler", "pathInfo"]
          , W.queryString    = []
          , W.requestBody    = mempty
          , W.vault          = mempty
          }
      fakeRequest =
        Request
          { reqGetParams  = []
          , reqCookies    = []
          , reqWaiRequest = fakeWaiRequest
          , reqLangs      = []
          , reqToken      = Just "NaN" -- not a nonce =)
          , reqBodySize   = 0
          }
      fakeContentType = []
  _ <- runResourceT $ yapp errHandler fakeRequest fakeContentType fakeSessionMap
  I.readIORef ret
{-# WARNING runFakeHandler "Usually you should *not* use runFakeHandler unless you really understand how it works and why you need it." #-}
