{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
module Yesod.Core.Types where

import Data.Aeson (ToJSON)
import qualified Data.ByteString.Builder            as BB
import           Control.Arrow                      (first)
import           Control.Exception                  (Exception)
import           Control.Monad                      (ap)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Logger               (LogLevel, LogSource,
                                                     MonadLogger (..))
import           Control.Monad.Primitive            (PrimMonad (..))
import           Control.Monad.Trans.Resource       (MonadResource (..), InternalState, runInternalState, MonadThrow (..), ResourceT)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy               as L
import           Data.CaseInsensitive               (CI)
import           Data.Conduit                       (Flush, ConduitT)
import           Data.IORef                         (IORef, modifyIORef')
import           Data.Map                           (Map, unionWith)
import qualified Data.Map                           as Map
import           Data.Monoid                        (Endo (..), Last (..))
import           Data.Semigroup                     (Semigroup(..))
import           Data.Serialize                     (Serialize (..),
                                                     putByteString)
import           Data.String                        (IsString (fromString))
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Lazy.Builder             as TBuilder
import           Data.Time                          (UTCTime)
import           GHC.Generics                       (Generic)
import           Language.Haskell.TH.Syntax         (Loc)
import qualified Network.HTTP.Types                 as H
import           Network.Wai                        (FilePart,
                                                     RequestBodyLength)
import qualified Network.Wai                        as W
import qualified Network.Wai.Parse                  as NWP
import           System.Log.FastLogger              (LogStr, LoggerSet, toLogStr, pushLogStr)
import           Network.Wai.Logger                 (DateCacheGetter)
import           Text.Blaze.Html                    (Html, toHtml)
import           Text.Hamlet                        (HtmlUrl)
import           Text.Julius                        (JavascriptUrl)
import           Web.Cookie                         (SetCookie)
import           Yesod.Core.Internal.Util           (getTime, putTime)
import           Yesod.Routes.Class                 (RenderRoute (..), ParseRoute (..))
import           Control.Monad.Reader               (MonadReader (..))
import Control.DeepSeq (NFData (rnf))
import Yesod.Core.TypeCache (TypeMap, KeyedTypeMap)
import Control.Monad.Logger (MonadLoggerIO (..))
import UnliftIO (MonadUnliftIO (..))

-- Sessions
type SessionMap = Map Text ByteString

type SaveSession = SessionMap -- ^ The session contents after running the handler
                -> IO [Header]

newtype SessionBackend = SessionBackend
    { sbLoadSession :: W.Request
                    -> IO (SessionMap, SaveSession) -- ^ Return the session data and a function to save the session
    }

data SessionCookie = SessionCookie !(Either UTCTime ByteString) !ByteString !SessionMap
    deriving (Show, Read)
instance Serialize SessionCookie where
    put (SessionCookie a b c) = do
        either putTime putByteString a
        put b
        put (map (first T.unpack) $ Map.toList c)

    get = do
        a <- getTime
        b <- get
        c <- map (first T.pack) <$> get
        return $ SessionCookie (Left a) b (Map.fromList c)

data ClientSessionDateCache =
  ClientSessionDateCache {
    csdcNow               :: !UTCTime
  , csdcExpires           :: !UTCTime
  , csdcExpiresSerialized :: !ByteString
  } deriving (Eq, Show)

-- | The parsed request information. This type augments the standard WAI
-- 'W.Request' with additional information.
data YesodRequest = YesodRequest
    { reqGetParams  :: ![(Text, Text)]
      -- ^ Same as 'W.queryString', but decoded to @Text@.
    , reqCookies    :: ![(Text, Text)]
    , reqWaiRequest :: !W.Request
    , reqLangs      :: ![Text]
      -- ^ Languages which the client supports. This is an ordered list by preference.
    , reqToken      :: !(Maybe Text)
      -- ^ A random, session-specific token used to prevent CSRF attacks.
    , reqSession    :: !SessionMap
      -- ^ Initial session sent from the client.
      --
      -- Since 1.2.0
    , reqAccept     :: ![ContentType]
      -- ^ An ordered list of the accepted content types.
      --
      -- Since 1.2.0
    }

-- | An augmented WAI 'W.Response'. This can either be a standard @Response@,
-- or a higher-level data structure which Yesod will turn into a @Response@.
data YesodResponse
    = YRWai !W.Response
    | YRWaiApp !W.Application
    | YRPlain !H.Status ![Header] !ContentType !Content !SessionMap

-- | A tuple containing both the POST parameters and submitted files.
type RequestBodyContents =
    ( [(Text, Text)]
    , [(Text, FileInfo)]
    )

data FileInfo = FileInfo
    { fileName        :: !Text
    , fileContentType :: !Text
    , fileSourceRaw   :: !(ConduitT () ByteString (ResourceT IO) ())
    , fileMove        :: !(FilePath -> IO ())
    }

data FileUpload = FileUploadMemory !(NWP.BackEnd L.ByteString)
                | FileUploadDisk !(InternalState -> NWP.BackEnd FilePath)
                | FileUploadSource !(NWP.BackEnd (ConduitT () ByteString (ResourceT IO) ()))

-- | How to determine the root of the application for constructing URLs.
--
-- Note that future versions of Yesod may add new constructors without bumping
-- the major version number. As a result, you should /not/ pattern match on
-- @Approot@ values.
data Approot master = ApprootRelative -- ^ No application root.
                    | ApprootStatic !Text
                    | ApprootMaster !(master -> Text)
                    | ApprootRequest !(master -> W.Request -> Text)

type ResolvedApproot = Text

data AuthResult = Authorized | AuthenticationRequired | Unauthorized !Text
    deriving (Eq, Show, Read)

data ScriptLoadPosition master
    = BottomOfBody
    | BottomOfHeadBlocking
    | BottomOfHeadAsync !(BottomOfHeadAsync master)

type BottomOfHeadAsync master
       = [Text] -- ^ urls to load asynchronously
      -> Maybe (HtmlUrl (Route master)) -- ^ widget of js to run on async completion
      -> HtmlUrl (Route master) -- ^ widget to insert at the bottom of <head>

type Texts = [Text]

-- | Wrap up a normal WAI application as a Yesod subsite. Ignore parent site's middleware and isAuthorized.
newtype WaiSubsite = WaiSubsite { runWaiSubsite :: W.Application }

-- | Like 'WaiSubsite', but applies parent site's middleware and isAuthorized.
--
-- @since 1.4.34
newtype WaiSubsiteWithAuth = WaiSubsiteWithAuth { runWaiSubsiteWithAuth :: W.Application }

data RunHandlerEnv child site = RunHandlerEnv
    { rheRender   :: !(Route site -> [(Text, Text)] -> Text)
    , rheRoute    :: !(Maybe (Route child))
    , rheRouteToMaster :: !(Route child -> Route site)
    , rheSite     :: !site
    , rheChild    :: !child
    , rheUpload   :: !(RequestBodyLength -> FileUpload)
    , rheLog      :: !(Loc -> LogSource -> LogLevel -> LogStr -> IO ())
    , rheOnError  :: !(ErrorResponse -> YesodApp)
      -- ^ How to respond when an error is thrown internally.
      --
      -- Since 1.2.0
    , rheMaxExpires :: !Text
    }

data HandlerData child site = HandlerData
    { handlerRequest  :: !YesodRequest
    , handlerEnv      :: !(RunHandlerEnv child site)
    , handlerState    :: !(IORef GHState)
    , handlerResource :: !InternalState
    }

data YesodRunnerEnv site = YesodRunnerEnv
    { yreLogger         :: !Logger
    , yreSite           :: !site
    , yreSessionBackend :: !(Maybe SessionBackend)
    , yreGen            :: !(IO Int)
    -- ^ Generate a random number uniformly distributed in the full
    -- range of 'Int'.
    --
    -- Note: Before 1.6.20, the default value generates pseudo-random
    -- number in an unspecified range. The range size may not be a power
    -- of 2. Since 1.6.20, the default value uses a secure entropy source
    -- and generates in the full range of 'Int'.
    , yreGetMaxExpires  :: !(IO Text)
    }

data YesodSubRunnerEnv sub parent = YesodSubRunnerEnv
    { ysreParentRunner  :: !(ParentRunner parent)
    , ysreGetSub        :: !(parent -> sub)
    , ysreToParentRoute :: !(Route sub -> Route parent)
    , ysreParentEnv     :: !(YesodRunnerEnv parent) -- FIXME maybe get rid of this and remove YesodRunnerEnv in ParentRunner?
    }

type ParentRunner parent
    = HandlerFor parent TypedContent
   -> YesodRunnerEnv parent
   -> Maybe (Route parent)
   -> W.Application

-- | A generic handler monad, which can have a different subsite and master
-- site. We define a newtype for better error message.
newtype HandlerFor site a = HandlerFor
    { unHandlerFor :: HandlerData site site -> IO a
    }
    deriving Functor

data GHState = GHState
    { ghsSession :: !SessionMap
    , ghsRBC     :: !(Maybe RequestBodyContents)
    , ghsIdent   :: !Int
    , ghsCache   :: !TypeMap
    , ghsCacheBy :: !KeyedTypeMap
    , ghsHeaders :: !(Endo [Header])
    }

-- | An extension of the basic WAI 'W.Application' datatype to provide extra
-- features needed by Yesod. Users should never need to use this directly, as
-- the 'HandlerFor' monad and template haskell code should hide it away.
type YesodApp = YesodRequest -> ResourceT IO YesodResponse

-- | A generic widget, allowing specification of both the subsite and master
-- site datatypes. While this is simply a @WriterT@, we define a newtype for
-- better error messages.
newtype WidgetFor site a = WidgetFor
    { unWidgetFor :: WidgetData site -> IO a
    }
    deriving Functor

data WidgetData site = WidgetData
  { wdRef :: {-# UNPACK #-} !(IORef (GWData (Route site)))
  , wdHandler :: {-# UNPACK #-} !(HandlerData site site)
  }

instance a ~ () => Monoid (WidgetFor site a) where
    mempty = return ()
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif
instance a ~ () => Semigroup (WidgetFor site a) where
    x <> y = x >> y

-- | A 'String' can be trivially promoted to a widget.
--
-- For example, in a yesod-scaffold site you could use:
--
-- @getHomeR = do defaultLayout "Widget text"@
instance a ~ () => IsString (WidgetFor site a) where
    fromString = toWidget . toHtml . T.pack
      where toWidget x = tellWidget mempty { gwdBody = Body (const x) }

tellWidget :: GWData (Route site) -> WidgetFor site ()
tellWidget d = WidgetFor $ \wd -> modifyIORef' (wdRef wd) (<> d)

type RY master = Route master -> [(Text, Text)] -> Text

-- | Newtype wrapper allowing injection of arbitrary content into CSS.
--
-- Usage:
--
-- > toWidget $ CssBuilder "p { color: red }"
--
-- Since: 1.1.3
newtype CssBuilder = CssBuilder { unCssBuilder :: TBuilder.Builder }

-- | Content for a web page. By providing this datatype, we can easily create
-- generic site templates, which would have the type signature:
--
-- > PageContent url -> HtmlUrl url
data PageContent url = PageContent
    { pageTitle       :: !Html
    , pageDescription :: !(Maybe Text)
    , pageHead        :: !(HtmlUrl url)
    , pageBody        :: !(HtmlUrl url)
    }

data Content = ContentBuilder !BB.Builder !(Maybe Int) -- ^ The content and optional content length.
             | ContentSource !(ConduitT () (Flush BB.Builder) (ResourceT IO) ())
             | ContentFile !FilePath !(Maybe FilePart)
             | ContentDontEvaluate !Content

data TypedContent = TypedContent !ContentType !Content

type RepHtml = Html
{-# DEPRECATED RepHtml "Please use Html instead" #-}
newtype RepJson = RepJson Content
newtype RepPlain = RepPlain Content
newtype RepXml = RepXml Content

type ContentType = ByteString -- FIXME Text?

-- | Wrapper around types so that Handlers can return a domain type, even when
-- the data will eventually be encoded as JSON.
-- Example usage in a type signature:
--
-- > postSignupR :: Handler (JSONResponse CreateUserResponse)
--
-- And in the implementation:
--
-- > return $ JSONResponse $ CreateUserResponse userId
--
-- @since 1.6.14
data JSONResponse a where
    JSONResponse :: ToJSON a => a -> JSONResponse a

-- | Prevents a response body from being fully evaluated before sending the
-- request.
--
-- Since 1.1.0
newtype DontFullyEvaluate a = DontFullyEvaluate { unDontFullyEvaluate :: a }

-- | Responses to indicate some form of an error occurred.
data ErrorResponse =
      NotFound
        -- ^ The requested resource was not found.
        -- Examples of when this occurs include when an incorrect URL is used, or @yesod-persistent@'s 'get404' doesn't find a value.
        -- HTTP status: 404.
    | InternalError !Text
        -- ^ Some sort of unexpected exception.
        -- If your application uses `throwIO` or `error` to throw an exception, this is the form it would take.
        -- HTTP status: 500.
    | InvalidArgs ![Text]
        -- ^ Indicates some sort of invalid or missing argument, like a missing query parameter or malformed JSON body.
        -- Examples Yesod functions that send this include 'requireCheckJsonBody' and @Yesod.Auth.GoogleEmail2@.
        -- HTTP status: 400.
    | NotAuthenticated
        -- ^ Indicates the user is not logged in.
        -- This is thrown when 'isAuthorized' returns 'AuthenticationRequired'.
        -- HTTP code: 401.
    | PermissionDenied !Text
        -- ^ Indicates the user doesn't have permission to access the requested resource.
        -- This is thrown when 'isAuthorized' returns 'Unauthorized'.
        -- HTTP code: 403.
    | BadMethod !H.Method
        -- ^ Indicates the URL would have been valid if used with a different HTTP method (e.g. a GET was used, but only POST is handled.)
        -- HTTP code: 405.
    deriving (Show, Eq, Generic)
instance NFData ErrorResponse

----- header stuff
-- | Headers to be added to a 'Result'.
data Header =
      AddCookie !SetCookie
    | DeleteCookie !ByteString !ByteString
    -- ^ name and path
    | Header !(CI ByteString) !ByteString
    -- ^ key and value
    deriving (Eq, Show)

-- FIXME In the next major version bump, let's just add strictness annotations
-- to Header (and probably everywhere else). We can also add strictness
-- annotations to SetCookie in the cookie package.
instance NFData Header where
    rnf (AddCookie x) = rnf x
    rnf (DeleteCookie x y) = x `seq` y `seq` ()
    rnf (Header x y) = x `seq` y `seq` ()

data Location url = Local !url | Remote !Text
    deriving (Show, Eq)

-- | A diff list that does not directly enforce uniqueness.
-- When creating a widget Yesod will use nub to make it unique.
newtype UniqueList x = UniqueList ([x] -> [x])

data Script url = Script { scriptLocation :: !(Location url), scriptAttributes :: ![(Text, Text)] }
    deriving (Show, Eq)
data Stylesheet url = Stylesheet { styleLocation :: !(Location url), styleAttributes :: ![(Text, Text)] }
    deriving (Show, Eq)
newtype Title = Title { unTitle :: Html }
newtype Description = Description { unDescription :: Text }

newtype Head url = Head (HtmlUrl url)
    deriving Monoid
instance Semigroup (Head url) where
  (<>) = mappend
newtype Body url = Body (HtmlUrl url)
    deriving Monoid
instance Semigroup (Body url) where
  (<>) = mappend

type CssBuilderUrl a = (a -> [(Text, Text)] -> Text) -> TBuilder.Builder

data GWData a = GWData
    { gwdBody        :: !(Body a)
    , gwdTitle       :: !(Last Title)
    , gwdDescription :: !(Last Description)
    , gwdScripts     :: !(UniqueList (Script a))
    , gwdStylesheets :: !(UniqueList (Stylesheet a))
    , gwdCss         :: !(Map (Maybe Text) (CssBuilderUrl a)) -- media type
    , gwdJavascript  :: !(Maybe (JavascriptUrl a))
    , gwdHead        :: !(Head a)
    }
instance Monoid (GWData a) where
    mempty = GWData mempty mempty mempty mempty mempty mempty mempty mempty
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif
instance Semigroup (GWData a) where
    GWData a1 a2 a3 a4 a5 a6 a7 a8 <>
      GWData b1 b2 b3 b4 b5 b6 b7 b8 = GWData
        (mappend a1 b1)
        (mappend a2 b2)
        (mappend a3 b3)
        (mappend a4 b4)
        (mappend a5 b5)
        (unionWith mappend a6 b6)
        (mappend a7 b7)
        (mappend a8 b8)

data HandlerContents =
      HCContent !H.Status !TypedContent
    | HCError !ErrorResponse
    | HCSendFile !ContentType !FilePath !(Maybe FilePart)
    | HCRedirect !H.Status !Text
    | HCCreated !Text
    | HCWai !W.Response
    | HCWaiApp !W.Application

instance Show HandlerContents where
    show (HCContent status (TypedContent t _)) = "HCContent " ++ show (status, t)
    show (HCError e) = "HCError " ++ show e
    show (HCSendFile ct fp mfp) = "HCSendFile " ++ show (ct, fp, mfp)
    show (HCRedirect s t) = "HCRedirect " ++ show (s, t)
    show (HCCreated t) = "HCCreated " ++ show t
    show (HCWai _) = "HCWai"
    show (HCWaiApp _) = "HCWaiApp"
instance Exception HandlerContents

-- Instances for WidgetFor
instance Applicative (WidgetFor site) where
    pure = WidgetFor . const . pure
    (<*>) = ap
instance Monad (WidgetFor site) where
    return = pure
    WidgetFor x >>= f = WidgetFor $ \wd -> do
        a <- x wd
        unWidgetFor (f a) wd
instance MonadIO (WidgetFor site) where
    liftIO = WidgetFor . const
-- | @since 1.6.7
instance PrimMonad (WidgetFor site) where
    type PrimState (WidgetFor site) = PrimState IO
    primitive = liftIO . primitive
-- | @since 1.4.38
instance MonadUnliftIO (WidgetFor site) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner = WidgetFor $ \x -> inner $ flip unWidgetFor x
instance MonadReader (WidgetData site) (WidgetFor site) where
    ask = WidgetFor return
    local f (WidgetFor g) = WidgetFor $ g . f

instance MonadThrow (WidgetFor site) where
    throwM = liftIO . throwM

instance MonadResource (WidgetFor site) where
    liftResourceT f = WidgetFor $ runInternalState f . handlerResource . wdHandler

instance MonadLogger (WidgetFor site) where
    monadLoggerLog a b c d = WidgetFor $ \wd ->
        rheLog (handlerEnv $ wdHandler wd) a b c (toLogStr d)

instance MonadLoggerIO (WidgetFor site) where
    askLoggerIO = WidgetFor $ return . rheLog . handlerEnv . wdHandler

-- Instances for HandlerFor
instance Applicative (HandlerFor site) where
    pure = HandlerFor . const . return
    (<*>) = ap
instance Monad (HandlerFor site) where
    return = pure
    HandlerFor x >>= f = HandlerFor $ \r -> x r >>= \x' -> unHandlerFor (f x') r
instance MonadIO (HandlerFor site) where
    liftIO = HandlerFor . const
-- | @since 1.6.7
instance PrimMonad (HandlerFor site) where
    type PrimState (HandlerFor site) = PrimState IO
    primitive = liftIO . primitive
instance MonadReader (HandlerData site site) (HandlerFor site) where
    ask = HandlerFor return
    local f (HandlerFor g) = HandlerFor $ g . f

-- | @since 1.4.38
instance MonadUnliftIO (HandlerFor site) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner = HandlerFor $ \x -> inner $ flip unHandlerFor x

instance MonadThrow (HandlerFor site) where
    throwM = liftIO . throwM

instance MonadResource (HandlerFor site) where
    liftResourceT f = HandlerFor $ runInternalState f . handlerResource

instance MonadLogger (HandlerFor site) where
    monadLoggerLog a b c d = HandlerFor $ \hd ->
        rheLog (handlerEnv hd) a b c (toLogStr d)

instance MonadLoggerIO (HandlerFor site) where
    askLoggerIO = HandlerFor $ \hd -> return (rheLog (handlerEnv hd))

instance Monoid (UniqueList x) where
    mempty = UniqueList id
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif
instance Semigroup (UniqueList x) where
    UniqueList x <> UniqueList y = UniqueList $ x . y

instance IsString Content where
    fromString = flip ContentBuilder Nothing . BB.stringUtf8

instance RenderRoute WaiSubsite where
    data Route WaiSubsite = WaiSubsiteRoute [Text] [(Text, Text)]
        deriving (Show, Eq, Read, Ord)
    renderRoute (WaiSubsiteRoute ps qs) = (ps, qs)
instance ParseRoute WaiSubsite where
    parseRoute (x, y) = Just $ WaiSubsiteRoute x y

instance RenderRoute WaiSubsiteWithAuth where
  data Route WaiSubsiteWithAuth = WaiSubsiteWithAuthRoute [Text] [(Text,Text)]
       deriving (Show, Eq, Read, Ord)
  renderRoute (WaiSubsiteWithAuthRoute ps qs) = (ps,qs)

instance ParseRoute WaiSubsiteWithAuth where
  parseRoute (x, y) = Just $ WaiSubsiteWithAuthRoute x y

data Logger = Logger
    { loggerSet :: !LoggerSet
    , loggerDate :: !DateCacheGetter
    }

loggerPutStr :: Logger -> LogStr -> IO ()
loggerPutStr (Logger ls _) = pushLogStr ls

-- | A handler monad for subsite
--
-- @since 1.6.0
newtype SubHandlerFor sub master a = SubHandlerFor
    { unSubHandlerFor :: HandlerData sub master -> IO a
    }
    deriving Functor

instance Applicative (SubHandlerFor child master) where
    pure = SubHandlerFor . const . return
    (<*>) = ap
instance Monad (SubHandlerFor child master) where
    return = pure
    SubHandlerFor x >>= f = SubHandlerFor $ \r -> x r >>= \x' -> unSubHandlerFor (f x') r
instance MonadIO (SubHandlerFor child master) where
    liftIO = SubHandlerFor . const
instance MonadReader (HandlerData child master) (SubHandlerFor child master) where
    ask = SubHandlerFor return
    local f (SubHandlerFor g) = SubHandlerFor $ g . f

-- | @since 1.4.38
instance MonadUnliftIO (SubHandlerFor child master) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner = SubHandlerFor $ \x -> inner $ flip unSubHandlerFor x

instance MonadThrow (SubHandlerFor child master) where
    throwM = liftIO . throwM

instance MonadResource (SubHandlerFor child master) where
    liftResourceT f = SubHandlerFor $ runInternalState f . handlerResource

instance MonadLogger (SubHandlerFor child master) where
    monadLoggerLog a b c d = SubHandlerFor $ \sd ->
        rheLog (handlerEnv sd) a b c (toLogStr d)

instance MonadLoggerIO (SubHandlerFor child master) where
    askLoggerIO = SubHandlerFor $ return . rheLog . handlerEnv
