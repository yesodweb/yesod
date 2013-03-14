{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances #-}
module Yesod.Core.Types where

import qualified Blaze.ByteString.Builder           as BBuilder
import qualified Blaze.ByteString.Builder.Char.Utf8
import           Control.Applicative                (Applicative (..))
import           Control.Applicative                ((<$>))
import           Control.Arrow                      (first)
import           Control.Exception                  (Exception, throwIO)
import           Control.Failure                    (Failure (..))
import           Control.Monad                      (liftM, ap)
import           Control.Monad.Trans.Class          (MonadTrans)
import qualified Control.Monad.Trans.Class          as Trans
import           Control.Monad.Base                 (MonadBase (liftBase))
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Logger               (LogLevel, LogSource,
                                                     MonadLogger (..))
import           Control.Monad.Trans.Control        (MonadBaseControl (..))
import           Control.Monad.Trans.Resource       (MonadResource (..))
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy               as L
import           Data.Conduit                       (Flush, MonadThrow (..),
                                                     MonadUnsafeIO (..),
                                                     ResourceT, Source)
import           Data.Dynamic                       (Dynamic)
import           Data.IORef                         (IORef)
import           Data.Map                           (Map, unionWith)
import qualified Data.Map                           as Map
import           Data.Monoid                        (Endo (..), Last (..),
                                                     Monoid (..))
import           Data.Serialize                     (Serialize (..),
                                                     putByteString)
import           Data.String                        (IsString (fromString))
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Lazy.Builder             as TBuilder
import           Data.Time                          (UTCTime)
import           Data.Typeable                      (Typeable)
import           Data.Typeable                      (TypeRep)
import           Language.Haskell.TH.Syntax         (Loc)
import qualified Network.HTTP.Types                 as H
import           Network.Wai                        (FilePart,
                                                     RequestBodyLength)
import qualified Network.Wai                        as W
import qualified Network.Wai.Parse                  as NWP
import           System.Log.FastLogger              (LogStr, Logger, toLogStr)
import           Text.Blaze.Html                    (Html)
import           Text.Hamlet                        (HtmlUrl)
import           Text.Julius                        (JavascriptUrl)
import           Web.Cookie                         (SetCookie)
import           Yesod.Core.Internal.Util           (getTime, putTime)
import           Control.Monad.Trans.Class
import           Yesod.Routes.Class                 (RenderRoute (..))

-- Sessions
type SessionMap = Map Text ByteString

type SaveSession = SessionMap -- ^ The session contents after running the handler
                -> IO [Header]

newtype SessionBackend = SessionBackend
    { sbLoadSession :: W.Request
                    -> IO (SessionMap, SaveSession) -- ^ Return the session data and a function to save the session
    }

data SessionCookie = SessionCookie (Either UTCTime ByteString) ByteString SessionMap
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
    | YRPlain !H.Status ![Header] !ContentType !Content !SessionMap

-- | A tuple containing both the POST parameters and submitted files.
type RequestBodyContents =
    ( [(Text, Text)]
    , [(Text, FileInfo)]
    )

data FileInfo = FileInfo
    { fileName        :: !Text
    , fileContentType :: !Text
    , fileSource      :: !(Source (ResourceT IO) ByteString)
    , fileMove        :: !(FilePath -> IO ())
    }

data FileUpload = FileUploadMemory !(NWP.BackEnd L.ByteString)
                | FileUploadDisk !(NWP.BackEnd FilePath)
                | FileUploadSource !(NWP.BackEnd (Source (ResourceT IO) ByteString))

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

data AuthResult = Authorized | AuthenticationRequired | Unauthorized Text
    deriving (Eq, Show, Read)

data ScriptLoadPosition master
    = BottomOfBody
    | BottomOfHeadBlocking
    | BottomOfHeadAsync (BottomOfHeadAsync master)

type BottomOfHeadAsync master
       = [Text] -- ^ urls to load asynchronously
      -> Maybe (HtmlUrl (Route master)) -- ^ widget of js to run on async completion
      -> (HtmlUrl (Route master)) -- ^ widget to insert at the bottom of <head>

newtype Cache = Cache (Map TypeRep Dynamic)
    deriving Monoid

type Texts = [Text]

-- | Wrap up a normal WAI application as a Yesod subsite.
newtype WaiSubsite = WaiSubsite { runWaiSubsite :: W.Application }

data RunHandlerEnv site = RunHandlerEnv
    { rheRender   :: !(Route site -> [(Text, Text)] -> Text)
    , rheRoute    :: !(Maybe (Route site))
    , rheSite     :: !site
    , rheUpload   :: !(RequestBodyLength -> FileUpload)
    , rheLog      :: !(Loc -> LogSource -> LogLevel -> LogStr -> IO ())
    , rheOnError  :: !(ErrorResponse -> YesodApp)
      -- ^ How to respond when an error is thrown internally.
      --
      -- Since 1.2.0
    }

data HandlerData site parentRoute = HandlerData
    { handlerRequest  :: !YesodRequest
    , handlerEnv      :: !(RunHandlerEnv site)
    , handlerState    :: !(IORef GHState)
    , handlerToParent :: !(Route site -> parentRoute)
    }

data YesodRunnerEnv site = YesodRunnerEnv
    { yreLogger         :: !Logger
    , yreSite           :: !site
    , yreSessionBackend :: !(Maybe SessionBackend)
    }

-- | A generic handler monad, which can have a different subsite and master
-- site. We define a newtype for better error message.
newtype HandlerT site m a = HandlerT
    { unHandlerT :: HandlerData site (MonadRoute m) -> ResourceT m a
    }

type family MonadRoute (m :: * -> *)
type instance MonadRoute IO = ()
type instance MonadRoute (HandlerT site m) = (Route site)

data GHState = GHState
    { ghsSession :: SessionMap
    , ghsRBC     :: Maybe RequestBodyContents
    , ghsIdent   :: Int
    , ghsCache   :: Cache
    , ghsHeaders :: Endo [Header]
    }

-- | An extension of the basic WAI 'W.Application' datatype to provide extra
-- features needed by Yesod. Users should never need to use this directly, as
-- the 'HandlerT' monad and template haskell code should hide it away.
type YesodApp = YesodRequest -> ResourceT IO YesodResponse

-- | A generic widget, allowing specification of both the subsite and master
-- site datatypes. While this is simply a @WriterT@, we define a newtype for
-- better error messages.
newtype WidgetT site m a = WidgetT
    { unWidgetT :: HandlerT site m (a, GWData (Route site))
    }

instance (a ~ (), Monad m) => Monoid (WidgetT site m a) where
    mempty = return ()
    mappend x y = x >> y

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
    { pageTitle :: Html
    , pageHead  :: HtmlUrl url
    , pageBody  :: HtmlUrl url
    }

data Content = ContentBuilder !BBuilder.Builder !(Maybe Int) -- ^ The content and optional content length.
             | ContentSource !(Source (ResourceT IO) (Flush BBuilder.Builder))
             | ContentFile !FilePath !(Maybe FilePart)
             | ContentDontEvaluate !Content

data TypedContent = TypedContent !ContentType !Content

type RepHtml = Html
newtype RepJson = RepJson Content
newtype RepPlain = RepPlain Content
newtype RepXml = RepXml Content

type ContentType = ByteString -- FIXME Text?

-- | Prevents a response body from being fully evaluated before sending the
-- request.
--
-- Since 1.1.0
newtype DontFullyEvaluate a = DontFullyEvaluate { unDontFullyEvaluate :: a }

-- | Responses to indicate some form of an error occurred. These are different
-- from 'SpecialResponse' in that they allow for custom error pages.
data ErrorResponse =
      NotFound
    | InternalError Text
    | InvalidArgs [Text]
    | PermissionDenied Text
    | BadMethod H.Method
    deriving (Show, Eq, Typeable)

----- header stuff
-- | Headers to be added to a 'Result'.
data Header =
    AddCookie SetCookie
    | DeleteCookie ByteString ByteString
    | Header ByteString ByteString
    deriving (Eq, Show)

data Location url = Local url | Remote Text
    deriving (Show, Eq)

newtype UniqueList x = UniqueList ([x] -> [x])

data Script url = Script { scriptLocation :: Location url, scriptAttributes :: [(Text, Text)] }
    deriving (Show, Eq)
data Stylesheet url = Stylesheet { styleLocation :: Location url, styleAttributes :: [(Text, Text)] }
    deriving (Show, Eq)
newtype Title = Title { unTitle :: Html }

newtype Head url = Head (HtmlUrl url)
    deriving Monoid
newtype Body url = Body (HtmlUrl url)
    deriving Monoid

type CssBuilderUrl a = (a -> [(Text, Text)] -> Text) -> TBuilder.Builder

data GWData a = GWData
    { gwdBody        :: !(Body a)
    , gwdTitle       :: !(Last Title)
    , gwdScripts     :: !(UniqueList (Script a))
    , gwdStylesheets :: !(UniqueList (Stylesheet a))
    , gwdCss         :: !(Map (Maybe Text) (CssBuilderUrl a)) -- media type
    , gwdJavascript  :: !(Maybe (JavascriptUrl a))
    , gwdHead        :: !(Head a)
    }
instance Monoid (GWData a) where
    mempty = GWData mempty mempty mempty mempty mempty mempty mempty
    mappend (GWData a1 a2 a3 a4 a5 a6 a7)
            (GWData b1 b2 b3 b4 b5 b6 b7) = GWData
        (a1 `mappend` b1)
        (a2 `mappend` b2)
        (a3 `mappend` b3)
        (a4 `mappend` b4)
        (unionWith mappend a5 b5)
        (a6 `mappend` b6)
        (a7 `mappend` b7)

data HandlerContents =
      HCContent H.Status !TypedContent
    | HCError ErrorResponse
    | HCSendFile ContentType FilePath (Maybe FilePart)
    | HCRedirect H.Status Text
    | HCCreated Text
    | HCWai W.Response
    deriving Typeable

instance Show HandlerContents where
    show _ = "Cannot show a HandlerContents"
instance Exception HandlerContents

-- Instances for WidgetT
instance Monad m => Functor (WidgetT site m) where
    fmap = liftM
instance Monad m => Applicative (WidgetT site m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (WidgetT site m) where
    return a = WidgetT $ pure (a, mempty)
    WidgetT x >>= f = WidgetT $ do
        (a, wa) <- x
        (b, wb) <- unWidgetT (f a)
        return (b, wa `mappend` wb)
instance MonadIO m => MonadIO (WidgetT site m) where
    liftIO = lift . liftIO
instance MonadBase b m => MonadBase b (WidgetT site m) where
    liftBase = WidgetT . fmap (\a -> (a, mempty)) . liftBase
instance MonadBaseControl b m => MonadBaseControl b (WidgetT site m) where
    data StM (WidgetT site m) a =
        StW (StM (HandlerT site m) (a, GWData (Route site)))
    liftBaseWith f = WidgetT $ liftBaseWith $ \runInBase ->
        liftM (\x -> (x, mempty))
        (f $ liftM StW . runInBase . unWidgetT)
    restoreM (StW base) = WidgetT $ restoreM base

instance MonadTrans (WidgetT site) where
    lift = WidgetT . fmap (, mempty) . lift
instance MonadThrow m => MonadThrow (WidgetT site m) where
    monadThrow = lift . monadThrow
instance (Applicative m, MonadIO m, MonadUnsafeIO m, MonadThrow m) => MonadResource (WidgetT site m) where
    liftResourceT = WidgetT . fmap (, mempty) . liftResourceT

instance MonadIO m => MonadLogger (WidgetT site m) where
    monadLoggerLog a b c d = WidgetT $ fmap (, mempty) $ monadLoggerLog a b c d

instance MonadTrans (HandlerT site) where
    lift = HandlerT . const . lift

-- Instances for HandlerT
instance Monad m => Functor (HandlerT site m) where
    fmap = liftM
instance Monad m => Applicative (HandlerT site m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (HandlerT site m) where
    return = HandlerT . const . return
    HandlerT x >>= f = HandlerT $ \r -> x r >>= \x' -> unHandlerT (f x') r
instance MonadIO m => MonadIO (HandlerT site m) where
    liftIO = lift . liftIO
instance MonadBase b m => MonadBase b (HandlerT site m) where
    liftBase = lift . liftBase
-- | Note: although we provide a @MonadBaseControl@ instance, @lifted-base@'s
-- @fork@ function is incompatible with the underlying @ResourceT@ system.
-- Instead, if you must fork a separate thread, you should use
-- @resourceForkIO@.
--
-- Using fork usually leads to an exception that says
-- \"Control.Monad.Trans.Resource.register\': The mutable state is being accessed
-- after cleanup. Please contact the maintainers.\"
instance MonadBaseControl b m => MonadBaseControl b (HandlerT site m) where
    data StM (HandlerT site m) a = StH (StM (ResourceT m) a)
    liftBaseWith f = HandlerT $ \reader ->
        liftBaseWith $ \runInBase ->
            f $ liftM StH . runInBase . (\(HandlerT r) -> r reader)
    restoreM (StH base) = HandlerT $ const $ restoreM base

instance MonadThrow m => MonadThrow (HandlerT site m) where
    monadThrow = lift . monadThrow
instance (MonadIO m, MonadUnsafeIO m, MonadThrow m, Applicative m) => MonadResource (HandlerT site m) where
    liftResourceT = HandlerT . const . liftResourceT

instance MonadIO m => MonadLogger (HandlerT site m) where
    monadLoggerLog a b c d = HandlerT $ \hd ->
        liftIO $ rheLog (handlerEnv hd) a b c (toLogStr d)

instance Failure e m => Failure e (HandlerT site m) where
    failure = lift . failure

instance Monoid (UniqueList x) where
    mempty = UniqueList id
    UniqueList x `mappend` UniqueList y = UniqueList $ x . y

instance IsString Content where
    fromString = flip ContentBuilder Nothing . Blaze.ByteString.Builder.Char.Utf8.fromString

instance RenderRoute WaiSubsite where
    data Route WaiSubsite = WaiSubsiteRoute [Text] [(Text, Text)]
        deriving (Show, Eq, Read, Ord)
    renderRoute (WaiSubsiteRoute ps qs) = (ps, qs)
