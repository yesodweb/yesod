{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Yesod.Core.Types where

import qualified Blaze.ByteString.Builder           as BBuilder
import qualified Blaze.ByteString.Builder.Char.Utf8
import           Control.Applicative                (Applicative (..))
import           Control.Applicative                ((<$>))
import           Control.Arrow                      (first)
import           Control.Exception                  (Exception, throwIO)
import           Control.Failure                    (Failure (..))
import           Control.Monad                      (liftM)
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
import           Yesod.Core.Class.MonadLift         (MonadLift (..))
import           Yesod.Routes.Class                 (RenderRoute (..))

-- Sessions
type SessionMap = Map Text ByteString

type SaveSession = SessionMap -- ^ The session contents after running the handler
                -> IO [Header]

newtype SessionBackend master = SessionBackend
    { sbLoadSession :: master
                    -> W.Request
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

data RunHandlerEnv sub master = RunHandlerEnv
    { rheRender   :: !(Route master -> [(Text, Text)] -> Text)
    , rheRoute    :: !(Maybe (Route sub))
    , rheToMaster :: !(Route sub -> Route master)
    , rheMaster   :: !master
    , rheSub      :: !sub
    , rheUpload   :: !(RequestBodyLength -> FileUpload)
    , rheLog      :: !(Loc -> LogSource -> LogLevel -> LogStr -> IO ())
    , rheOnError  :: !(ErrorResponse -> YesodApp)
      -- ^ How to respond when an error is thrown internally.
      --
      -- Since 1.2.0
    }

data HandlerData sub master = HandlerData
    { handlerRequest :: !YesodRequest
    , handlerEnv     :: !(RunHandlerEnv sub master)
    , handlerState   :: !(IORef GHState)
    }

data YesodRunnerEnv sub master = YesodRunnerEnv
    { yreLogger         :: !Logger
    , yreMaster         :: !master
    , yreSub            :: !sub
    , yreRoute          :: !(Maybe (Route sub))
    , yreToMaster       :: !(Route sub -> Route master)
    , yreSessionBackend :: !(Maybe (SessionBackend master))
    }

-- | A generic handler monad, which can have a different subsite and master
-- site. We define a newtype for better error message.
newtype GHandler sub master a = GHandler
    { unGHandler :: HandlerData sub master -> ResourceT IO a
    }

data GHState = GHState
    { ghsSession :: SessionMap
    , ghsRBC     :: Maybe RequestBodyContents
    , ghsIdent   :: Int
    , ghsCache   :: Cache
    , ghsHeaders :: Endo [Header]
    }

-- | An extension of the basic WAI 'W.Application' datatype to provide extra
-- features needed by Yesod. Users should never need to use this directly, as
-- the 'GHandler' monad and template haskell code should hide it away.
type YesodApp = YesodRequest -> ResourceT IO YesodResponse

-- | A generic widget, allowing specification of both the subsite and master
-- site datatypes. While this is simply a @WriterT@, we define a newtype for
-- better error messages.
newtype GWidget sub master a = GWidget
    { unGWidget :: GHandler sub master (a, GWData (Route master))
    }

instance (a ~ ()) => Monoid (GWidget sub master a) where
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

-- Instances for GWidget
instance Functor (GWidget sub master) where
    fmap f (GWidget x) = GWidget (fmap (first f) x)
instance Applicative (GWidget sub master) where
    pure a = GWidget $ pure (a, mempty)
    GWidget f <*> GWidget v =
        GWidget $ k <$> f <*> v
      where
        k (a, wa) (b, wb) = (a b, wa `mappend` wb)
instance Monad (GWidget sub master) where
    return = pure
    GWidget x >>= f = GWidget $ do
        (a, wa) <- x
        (b, wb) <- unGWidget (f a)
        return (b, wa `mappend` wb)
instance MonadIO (GWidget sub master) where
    liftIO = GWidget . fmap (\a -> (a, mempty)) . liftIO
instance MonadBase IO (GWidget sub master) where
    liftBase = GWidget . fmap (\a -> (a, mempty)) . liftBase
instance MonadBaseControl IO (GWidget sub master) where
    data StM (GWidget sub master) a =
        StW (StM (GHandler sub master) (a, GWData (Route master)))
    liftBaseWith f = GWidget $ liftBaseWith $ \runInBase ->
        liftM (\x -> (x, mempty))
        (f $ liftM StW . runInBase . unGWidget)
    restoreM (StW base) = GWidget $ restoreM base

instance MonadUnsafeIO (GWidget sub master) where
    unsafeLiftIO = liftIO
instance MonadThrow (GWidget sub master) where
    monadThrow = liftIO . throwIO
instance MonadResource (GWidget sub master) where
    liftResourceT = lift . liftResourceT

instance MonadLogger (GWidget sub master) where
    monadLoggerLog a b c = lift . monadLoggerLog a b c

instance MonadLift (GHandler sub master) (GWidget sub master) where
    lift = GWidget . fmap (\x -> (x, mempty))

instance MonadLift (ResourceT IO) (GHandler sub master) where
    lift = GHandler . const

-- Instances for GHandler
instance Functor (GHandler sub master) where
    fmap f (GHandler x) = GHandler $ \r -> fmap f (x r)
instance Applicative (GHandler sub master) where
    pure = GHandler . const . pure
    GHandler f <*> GHandler x = GHandler $ \r -> f r <*> x r
instance Monad (GHandler sub master) where
    return = pure
    GHandler x >>= f = GHandler $ \r -> x r >>= \x' -> unGHandler (f x') r
instance MonadIO (GHandler sub master) where
    liftIO = GHandler . const . lift
instance MonadBase IO (GHandler sub master) where
    liftBase = GHandler . const . lift
-- | Note: although we provide a @MonadBaseControl@ instance, @lifted-base@'s
-- @fork@ function is incompatible with the underlying @ResourceT@ system.
-- Instead, if you must fork a separate thread, you should use
-- @resourceForkIO@.
--
-- Using fork usually leads to an exception that says
-- \"Control.Monad.Trans.Resource.register\': The mutable state is being accessed
-- after cleanup. Please contact the maintainers.\"
instance MonadBaseControl IO (GHandler sub master) where
    data StM (GHandler sub master) a = StH (StM (ResourceT IO) a)
    liftBaseWith f = GHandler $ \reader ->
        liftBaseWith $ \runInBase ->
            f $ liftM StH . runInBase . (\(GHandler r) -> r reader)
    restoreM (StH base) = GHandler $ const $ restoreM base

instance MonadUnsafeIO (GHandler sub master) where
    unsafeLiftIO = liftIO
instance MonadThrow (GHandler sub master) where
    monadThrow = liftIO . throwIO
instance MonadResource (GHandler sub master) where
    liftResourceT = lift . liftResourceT

instance MonadLogger (GHandler sub master) where
    monadLoggerLog a b c d = GHandler $ \hd ->
        liftIO $ rheLog (handlerEnv hd) a b c (toLogStr d)

instance Exception e => Failure e (GHandler sub master) where
    failure = liftIO . throwIO

instance Monoid (UniqueList x) where
    mempty = UniqueList id
    UniqueList x `mappend` UniqueList y = UniqueList $ x . y

instance IsString Content where
    fromString = flip ContentBuilder Nothing . Blaze.ByteString.Builder.Char.Utf8.fromString

instance RenderRoute WaiSubsite where
    data Route WaiSubsite = WaiSubsiteRoute [Text] [(Text, Text)]
        deriving (Show, Eq, Read, Ord)
    renderRoute (WaiSubsiteRoute ps qs) = (ps, qs)
