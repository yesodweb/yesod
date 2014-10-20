{-# LANGUAGE CPP #-}
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
import           Control.Exception                  (Exception)
import           Control.Monad                      (liftM, ap)
import           Control.Monad.Base                 (MonadBase (liftBase))
import           Control.Monad.Catch                (MonadCatch (..))
import           Control.Monad.Catch                (MonadMask (..))
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Logger               (LogLevel, LogSource,
                                                     MonadLogger (..))
import           Control.Monad.Trans.Control        (MonadBaseControl (..))
import           Control.Monad.Trans.Resource       (MonadResource (..), InternalState, runInternalState, MonadThrow (..), monadThrow, ResourceT)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy               as L
import           Data.Conduit                       (Flush, Source)
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
import           Language.Haskell.TH.Syntax         (Loc)
import qualified Network.HTTP.Types                 as H
import           Network.Wai                        (FilePart,
                                                     RequestBodyLength)
import qualified Network.Wai                        as W
import qualified Network.Wai.Parse                  as NWP
import           System.Log.FastLogger              (LogStr, LoggerSet, toLogStr, pushLogStr)
import qualified System.Random.MWC                  as MWC
import           Network.Wai.Logger                 (DateCacheGetter)
import           Text.Blaze.Html                    (Html)
import           Text.Hamlet                        (HtmlUrl)
import           Text.Julius                        (JavascriptUrl)
import           Web.Cookie                         (SetCookie)
import           Yesod.Core.Internal.Util           (getTime, putTime)
import           Control.Monad.Trans.Class          (MonadTrans (..))
import           Yesod.Routes.Class                 (RenderRoute (..), ParseRoute (..))
import           Control.Monad.Reader               (MonadReader (..))
#if !MIN_VERSION_base(4, 6, 0)
import Prelude hiding (catch)
#endif
import Control.DeepSeq (NFData (rnf))
import Data.Conduit.Lazy (MonadActive, monadActive)
import Yesod.Core.TypeCache (TypeMap, KeyedTypeMap)

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
    , fileSourceRaw   :: !(Source (ResourceT IO) ByteString)
    , fileMove        :: !(FilePath -> IO ())
    }

data FileUpload = FileUploadMemory !(NWP.BackEnd L.ByteString)
                | FileUploadDisk !(InternalState -> NWP.BackEnd FilePath)
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
    , handlerResource :: !InternalState
    }

data YesodRunnerEnv site = YesodRunnerEnv
    { yreLogger         :: !Logger
    , yreSite           :: !site
    , yreSessionBackend :: !(Maybe SessionBackend)
    , yreGen            :: !MWC.GenIO
    }

data YesodSubRunnerEnv sub parent parentMonad = YesodSubRunnerEnv
    { ysreParentRunner  :: !(ParentRunner parent parentMonad)
    , ysreGetSub        :: !(parent -> sub)
    , ysreToParentRoute :: !(Route sub -> Route parent)
    , ysreParentEnv     :: !(YesodRunnerEnv parent) -- FIXME maybe get rid of this and remove YesodRunnerEnv in ParentRunner?
    }

type ParentRunner parent m
    = m TypedContent
   -> YesodRunnerEnv parent
   -> Maybe (Route parent)
   -> W.Application

-- | A generic handler monad, which can have a different subsite and master
-- site. We define a newtype for better error message.
newtype HandlerT site m a = HandlerT
    { unHandlerT :: HandlerData site (MonadRoute m) -> m a
    }

type family MonadRoute (m :: * -> *)
type instance MonadRoute IO = ()
type instance MonadRoute (HandlerT site m) = (Route site)

data GHState = GHState
    { ghsSession :: SessionMap
    , ghsRBC     :: Maybe RequestBodyContents
    , ghsIdent   :: Int
    , ghsCache   :: TypeMap
    , ghsCacheBy :: KeyedTypeMap
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
    { unWidgetT :: HandlerData site (MonadRoute m) -> m (a, GWData (Route site))
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
{-# DEPRECATED RepHtml "Please use Html instead" #-}
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
    | NotAuthenticated
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

-- FIXME In the next major version bump, let's just add strictness annotations
-- to Header (and probably everywhere else). We can also add strictness
-- annotations to SetCookie in the cookie package.
instance NFData Header where
    rnf (AddCookie x) = rnf x
    rnf (DeleteCookie x y) = x `seq` y `seq` ()
    rnf (Header x y) = x `seq` y `seq` ()

data Location url = Local url | Remote Text
    deriving (Show, Eq)

-- | A diff list that does not directly enforce uniqueness.
-- When creating a widget Yesod will use nub to make it unique.
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
    | HCWaiApp W.Application
    deriving Typeable

instance Show HandlerContents where
    show (HCContent status (TypedContent t _)) = "HCContent " ++ show (status, t)
    show (HCError e) = "HCError " ++ show e
    show (HCSendFile ct fp mfp) = "HCSendFile " ++ show (ct, fp, mfp)
    show (HCRedirect s t) = "HCRedirect " ++ show (s, t)
    show (HCCreated t) = "HCCreated " ++ show t
    show (HCWai _) = "HCWai"
    show (HCWaiApp _) = "HCWaiApp"
instance Exception HandlerContents

-- Instances for WidgetT
instance Monad m => Functor (WidgetT site m) where
    fmap = liftM
instance Monad m => Applicative (WidgetT site m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (WidgetT site m) where
    return a = WidgetT $ const $ return (a, mempty)
    WidgetT x >>= f = WidgetT $ \r -> do
        (a, wa) <- x r
        (b, wb) <- unWidgetT (f a) r
        return (b, wa `mappend` wb)
instance MonadIO m => MonadIO (WidgetT site m) where
    liftIO = lift . liftIO
instance MonadBase b m => MonadBase b (WidgetT site m) where
    liftBase = WidgetT . const . liftBase . fmap (, mempty)
instance MonadBaseControl b m => MonadBaseControl b (WidgetT site m) where
    data StM (WidgetT site m) a = StW (StM m (a, GWData (Route site)))
    liftBaseWith f = WidgetT $ \reader' ->
        liftBaseWith $ \runInBase ->
            liftM (\x -> (x, mempty))
            (f $ liftM StW . runInBase . flip unWidgetT reader')
    restoreM (StW base) = WidgetT $ const $ restoreM base
instance Monad m => MonadReader site (WidgetT site m) where
    ask = WidgetT $ \hd -> return (rheSite $ handlerEnv hd, mempty)
    local f (WidgetT g) = WidgetT $ \hd -> g hd
        { handlerEnv = (handlerEnv hd)
            { rheSite = f $ rheSite $ handlerEnv hd
            }
        }

instance MonadTrans (WidgetT site) where
    lift = WidgetT . const . liftM (, mempty)
instance MonadThrow m => MonadThrow (WidgetT site m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (HandlerT site m) where
  catch (HandlerT m) c = HandlerT $ \r -> m r `catch` \e -> unHandlerT (c e) r
instance MonadMask m => MonadMask (HandlerT site m) where
  mask a = HandlerT $ \e -> mask $ \u -> unHandlerT (a $ q u) e
    where q u (HandlerT b) = HandlerT (u . b)
  uninterruptibleMask a =
    HandlerT $ \e -> uninterruptibleMask $ \u -> unHandlerT (a $ q u) e
      where q u (HandlerT b) = HandlerT (u . b)
instance MonadCatch m => MonadCatch (WidgetT site m) where
  catch (WidgetT m) c = WidgetT $ \r -> m r `catch` \e -> unWidgetT (c e) r
instance MonadMask m => MonadMask (WidgetT site m) where
  mask a = WidgetT $ \e -> mask $ \u -> unWidgetT (a $ q u) e
    where q u (WidgetT b) = WidgetT (u . b)
  uninterruptibleMask a =
    WidgetT $ \e -> uninterruptibleMask $ \u -> unWidgetT (a $ q u) e
      where q u (WidgetT b) = WidgetT (u . b)

instance (Applicative m, MonadIO m, MonadBase IO m, MonadThrow m) => MonadResource (WidgetT site m) where
    liftResourceT f = WidgetT $ \hd -> liftIO $ fmap (, mempty) $ runInternalState f (handlerResource hd)

instance MonadIO m => MonadLogger (WidgetT site m) where
    monadLoggerLog a b c d = WidgetT $ \hd ->
        liftIO $ fmap (, mempty) $ rheLog (handlerEnv hd) a b c (toLogStr d)

instance MonadActive m => MonadActive (WidgetT site m) where
    monadActive = lift monadActive
instance MonadActive m => MonadActive (HandlerT site m) where
    monadActive = lift monadActive

instance MonadTrans (HandlerT site) where
    lift = HandlerT . const

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
instance Monad m => MonadReader site (HandlerT site m) where
    ask = HandlerT $ return . rheSite . handlerEnv
    local f (HandlerT g) = HandlerT $ \hd -> g hd
        { handlerEnv = (handlerEnv hd)
            { rheSite = f $ rheSite $ handlerEnv hd
            }
        }
-- | Note: although we provide a @MonadBaseControl@ instance, @lifted-base@'s
-- @fork@ function is incompatible with the underlying @ResourceT@ system.
-- Instead, if you must fork a separate thread, you should use
-- @resourceForkIO@.
--
-- Using fork usually leads to an exception that says
-- \"Control.Monad.Trans.Resource.register\': The mutable state is being accessed
-- after cleanup. Please contact the maintainers.\"
instance MonadBaseControl b m => MonadBaseControl b (HandlerT site m) where
    data StM (HandlerT site m) a = StH (StM m a)
    liftBaseWith f = HandlerT $ \reader' ->
        liftBaseWith $ \runInBase ->
            f $ liftM StH . runInBase . (\(HandlerT r) -> r reader')
    restoreM (StH base) = HandlerT $ const $ restoreM base

instance MonadThrow m => MonadThrow (HandlerT site m) where
    throwM = lift . monadThrow

instance (MonadIO m, MonadBase IO m, MonadThrow m) => MonadResource (HandlerT site m) where
    liftResourceT f = HandlerT $ \hd -> liftIO $ runInternalState f (handlerResource hd)

instance MonadIO m => MonadLogger (HandlerT site m) where
    monadLoggerLog a b c d = HandlerT $ \hd ->
        liftIO $ rheLog (handlerEnv hd) a b c (toLogStr d)

instance Monoid (UniqueList x) where
    mempty = UniqueList id
    UniqueList x `mappend` UniqueList y = UniqueList $ x . y

instance IsString Content where
    fromString = flip ContentBuilder Nothing . Blaze.ByteString.Builder.Char.Utf8.fromString

instance RenderRoute WaiSubsite where
    data Route WaiSubsite = WaiSubsiteRoute [Text] [(Text, Text)]
        deriving (Show, Eq, Read, Ord)
    renderRoute (WaiSubsiteRoute ps qs) = (ps, qs)
instance ParseRoute WaiSubsite where
    parseRoute (x, y) = Just $ WaiSubsiteRoute x y

data Logger = Logger
    { loggerSet :: !LoggerSet
    , loggerDate :: !DateCacheGetter
    }

loggerPutStr :: Logger -> LogStr -> IO ()
loggerPutStr (Logger ls _) = pushLogStr ls
