{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Yesod.Core.Class where

import           Control.Monad.Logger               (logErrorS)
import           Yesod.Content
import           Yesod.Handler                      hiding (getExpires)

import           Yesod.Routes.Class

import           Blaze.ByteString.Builder           (Builder)
import           Blaze.ByteString.Builder.Char.Utf8 (fromText)
import           Control.Arrow                      ((***))
import           Control.Monad                      (forM)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Logger               (LogLevel (LevelInfo, LevelOther),
                                                     LogSource)
import qualified Data.ByteString.Char8              as S8
import qualified Data.ByteString.Lazy               as L
import           Data.List                          (foldl')
import           Data.List                          (nub)
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE
import qualified Data.Text.Encoding.Error           as TEE
import           Data.Text.Lazy.Builder             (toLazyText)
import           Data.Text.Lazy.Encoding            (encodeUtf8)
import           Data.Word                          (Word64)
import           Language.Haskell.TH.Syntax         (Loc (..))
import           Network.HTTP.Types                 (encodePath)
import qualified Network.Wai                        as W
import           Network.Wai.Middleware.Gzip        (GzipSettings, def)
import           Network.Wai.Parse                  (lbsBackEnd,
                                                     tempFileBackEnd)
import           System.IO                          (stdout)
import           System.Log.FastLogger              (LogStr (..), Logger,
                                                     loggerDate, loggerPutStr,
                                                     mkLogger)
import           System.Log.FastLogger.Date         (ZonedDate)
import           Text.Blaze                         (customAttribute, textTag,
                                                     toValue, (!))
import           Text.Blaze                         (preEscapedToMarkup)
import qualified Text.Blaze.Html5                   as TBH
import           Text.Hamlet
import           Text.Julius
import qualified Web.ClientSession                  as CS
import           Web.Cookie                         (parseCookies)
import           Web.Cookie                         (SetCookie (..))
import           Yesod.Core.Types
import           Yesod.Internal.Session
import           Yesod.Widget

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
    errorHandler :: ErrorResponse -> GHandler sub a TypedContent
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
    makeSessionBackend _ = fmap Just defaultClientSessionBackend

    -- | How to store uploaded files.
    --
    -- Default: When the request body is greater than 50kb, store in a temp
    -- file. For chunked request bodies, store in a temp file. Otherwise, store
    -- in memory.
    fileUpload :: a -> W.RequestBodyLength -> FileUpload
    fileUpload _ (W.KnownLength size)
        | size <= 50000 = FileUploadMemory lbsBackEnd
    fileUpload _ _ = FileUploadDisk tempFileBackEnd

    -- | Should we log the given log source/level combination.
    --
    -- Default: Logs everything at or above 'logLevel'
    shouldLog :: a -> LogSource -> LogLevel -> Bool
    shouldLog a _ level = level >= logLevel a

    -- | A Yesod middleware, which will wrap every handler function. This
    -- allows you to run code before and after a normal handler.
    --
    -- Default: Adds the response header \"Vary: Accept, Accept-Language\" and
    -- performs authorization checks.
    --
    -- Since: 1.1.6
    yesodMiddleware :: GHandler sub a res -> GHandler sub a res
    yesodMiddleware handler = do
        setHeader "Vary" "Accept, Accept-Language"
        route <- getCurrentRoute
        case route of
            Nothing -> handler
            Just url -> do
                isWrite <- isWriteRequest url
                ar <- isAuthorized url isWrite
                case ar of
                    Authorized -> return ()
                    AuthenticationRequired -> do
                        master <- getYesod
                        case authRoute master of
                            Nothing ->
                                permissionDenied "Authentication required"
                            Just url' -> do
                                setUltDestCurrent
                                redirect url'
                    Unauthorized s' -> permissionDenied s'
                handler

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

    runUniqueList :: Eq x => UniqueList x -> [x]
    runUniqueList (UniqueList x) = nub $ x []

-- | Helper function for 'defaultErrorHandler'.
applyLayout' :: Yesod master
             => Html -- ^ title
             -> HtmlUrl (Route master) -- ^ body
             -> GHandler sub master TypedContent
applyLayout' title body = fmap toTypedContent $ defaultLayout $ do
    setTitle title
    toWidget body

-- | The default error handler for 'errorHandler'.
defaultErrorHandler :: Yesod y => ErrorResponse -> GHandler sub y TypedContent
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
defaultErrorHandler (InternalError e) = do
    $logErrorS "yesod-core" e
    applyLayout' "Internal Server Error"
        [hamlet|
$newline never
<h1>Internal Server Error
<pre>#{e}
|]
defaultErrorHandler (BadMethod m) =
    applyLayout' "Bad Method"
        [hamlet|
$newline never
<h1>Method Not Supported
<p>Method <code>#{S8.unpack m}</code> not supported
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

defaultClientSessionBackend :: Yesod master => IO (SessionBackend master)
defaultClientSessionBackend = do
  key <- CS.getKey CS.defaultKeyFile
  let timeout = fromIntegral (120 * 60 :: Int) -- 120 minutes
  (getCachedDate, _closeDateCacher) <- clientSessionDateCacher timeout
  return $ clientSessionBackend key getCachedDate

jsToHtml :: Javascript -> Html
jsToHtml (Javascript b) = preEscapedToMarkup $ toLazyText b

jelper :: JavascriptUrl url -> HtmlUrl url
jelper = fmap jsToHtml

left :: Either a b -> Maybe a
left (Left x) = Just x
left _ = Nothing

right :: Either a b -> Maybe b
right (Right x) = Just x
right _ = Nothing

clientSessionBackend :: Yesod master
                     => CS.Key  -- ^ The encryption key
                     -> IO ClientSessionDateCache -- ^ See 'clientSessionDateCacher'
                     -> SessionBackend master
clientSessionBackend key getCachedDate =
  SessionBackend {
    sbLoadSession = \master req -> loadClientSession key getCachedDate "_SESSION" master req
  }

loadClientSession :: Yesod master
                  => CS.Key
                  -> IO ClientSessionDateCache -- ^ See 'clientSessionDateCacher'
                  -> S8.ByteString -- ^ session name
                  -> master
                  -> W.Request
                  -> IO (SessionMap, SaveSession)
loadClientSession key getCachedDate sessionName master req = load
  where
    load = do
      date <- getCachedDate
      return (sess date, save date)
    sess date = fromMaybe Map.empty $ do
      raw <- lookup "Cookie" $ W.requestHeaders req
      val <- lookup sessionName $ parseCookies raw
      let host = "" -- fixme, properly lock sessions to client address
      decodeClientSession key date host val
    save date sess' = do
      -- We should never cache the IV!  Be careful!
      iv <- liftIO CS.randomIV
      return [AddCookie def
          { setCookieName = sessionName
          , setCookieValue = encodeClientSession key iv date host sess'
          , setCookiePath = Just (cookiePath master)
          , setCookieExpires = Just (csdcExpires date)
          , setCookieDomain = cookieDomain master
          , setCookieHttpOnly = True
          }]
        where
          host = "" -- fixme, properly lock sessions to client address

-- taken from file-location package
-- turn the TH Loc loaction information into a human readable string
-- leaving out the loc_end parameter
fileLocationToString :: Loc -> String
fileLocationToString loc = (loc_package loc) ++ ':' : (loc_module loc) ++
  ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
  where
    line = show . fst . loc_start
    char = show . snd . loc_start
