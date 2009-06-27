{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverlappingInstances #-}
---------------------------------------------------------
--
-- Module        : Web.Restful
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Lightweight framework for designing RESTful APIs.
--
---------------------------------------------------------
module Web.Restful
    (
      -- * Request parsing
      -- $param_overview

      -- ** Types
      ParamError
    , ParamName
    , ParamValue
      -- ** Parameter type class
    , Parameter (..)
      -- ** RequestParser helpers
    , getParam
    , postParam
    , urlParam
    , anyParam
    , cookieParam
    , identifier
    , acceptedLanguages
    , requestPath
      -- ** Building actual request
    , Request (..)
    , Hack.RequestMethod (..)
    , rawFiles
      -- * Response construction
    , Response (..)
    , response
      -- ** Helper 'Response' instances
      -- *** Generic hierarchichal text
    , Tree (..)
    , IsTree (..)
      -- *** Atom news feed
    , AtomFeed (..)
    , AtomFeedEntry (..)
      -- *** Sitemap
    , sitemap
    , SitemapUrl (..)
    , SitemapLoc (..)
    , SitemapChangeFreq (..)
      -- *** Generics
      -- **** List/detail
    , ListDetail (..)
    , ItemList (..)
    , ItemDetail (..)
    , -- **** Multiple response types.
      GenResponse (..)
      -- * Defining an application
    , ApplicationMonad
      -- ** Routing
    , addResource
      -- ** Settings
    , setHandler
    , setRpxnowApiKey
    , setUrlRewriter
    , setHtmlWrapper
      -- ** Engage
    , run
    ) where

-- hideously long import list
import qualified Hack
import qualified Hack.Handler.CGI
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State hiding (gets)
import Data.List (intercalate)
import Web.Encodings
import Data.Maybe (isJust)
import Data.ByteString.Class
import qualified Data.ByteString.Lazy as BS
import Data.Function.Predicate (equals)
import Data.Default
import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId
import Data.List.Split (splitOneOf)

import Hack.Middleware.Gzip
import Hack.Middleware.CleanPath
import Hack.Middleware.Jsonp
import Hack.Middleware.ClientSession

import Data.Time.Format
import Data.Time.Clock
import System.Locale
import Control.Applicative ((<$>), Applicative (..))
import Control.Arrow (second)

-- $param_overview
-- In Restful, all of the underlying parameter values are strings. They can
-- come from multiple sources: GET parameters, URL rewriting (FIXME: link),
-- cookies, etc. However, most applications eventually want to convert
-- those strings into something else, like 'Int's. Additionally, it is
-- often desirable to allow multiple values, or no value at all.
--
-- That is what the parameter concept is for. A 'Parameter' is any value
-- which can be converted from a 'String', or list of 'String's.

-- | Any kind of error message generated in the parsing stage.
type ParamError = String

-- | In GET parameters, the key. In cookies, the cookie name. So on and so
-- forth.
type ParamName = String

-- | The 'String' value of a parameter, such as cookie content.
type ParamValue = String

-- | Anything which can be converted from a 'String' or list of 'String's.
--
-- The default implementation of 'readParams' will error out if given
-- anything but 1 'ParamValue'. This is usually what you want.
--
-- Minimal complete definition: either 'readParam' or 'readParams'.
class Parameter a where
    -- | Convert a string into the desired value, or explain why that can't
    -- happen.
    readParam :: ParamValue -> Either ParamError a
    readParam = readParams . return

    -- | Convert a list of strings into the desired value, or explain why
    -- that can't happen.
    readParams :: [ParamValue] -> Either ParamError a
    readParams [x] = readParam x
    readParams [] = Left "Missing parameter"
    readParams xs = Left $ "Given " ++ show (length xs) ++
                           " values, expecting 1"

-- | Attempt to parse a list of param values using 'readParams'.
-- If that fails, return an error message and an undefined value. This way,
-- we can process all of the parameters and get all of the error messages.
-- Be careful not to use the value inside until you can be certain the
-- reading succeeded.
tryReadParams:: Parameter a
             => ParamName
             -> [ParamValue]
             -> RequestParser a
tryReadParams name params =
    case readParams params of
        Left s -> do
            tell [name ++ ": " ++ s]
            return $
              error $
                "Trying to evaluate nonpresent parameter " ++
                name
        Right x -> return x

-- | Helper function for generating 'RequestParser's from various
-- 'ParamValue' lists.
genParam :: Parameter a
         => (RawRequest -> ParamName -> [ParamValue])
         -> ParamName
         -> RequestParser a
genParam f name = do
    req <- ask
    tryReadParams name $ f req name

-- | Parse a value passed as a GET parameter.
getParam :: Parameter a => ParamName -> RequestParser a
getParam = genParam getParams

-- | Parse a value passed as a POST parameter.
postParam :: Parameter a => ParamName -> RequestParser a
postParam = genParam postParams

-- | Parse a value passed in the URL and extracted using rewrite.
-- (FIXME: link to rewrite section.)
urlParam :: Parameter a => ParamName -> RequestParser a
urlParam = genParam urlParams

-- | Parse a value passed as a GET, POST or URL parameter.
anyParam :: Parameter a => ParamName -> RequestParser a
anyParam = genParam anyParams

-- | Parse a value passed as a raw cookie.
cookieParam :: Parameter a => ParamName -> RequestParser a
cookieParam = genParam cookies

-- | Parse a value in the hackHeader field.
hackHeaderParam :: Parameter a => ParamName -> RequestParser a
hackHeaderParam name = do
    env <- parseEnv
    let vals' = lookup name $ Hack.hackHeaders env
        vals = case vals' of
                Nothing -> []
                Just x -> [x]
    tryReadParams name vals

-- | Extract the cookie which specifies the identifier for a logged in
-- user.
identifier :: Parameter a => RequestParser a
identifier = hackHeaderParam authCookieName

-- | Get the raw 'Hack.Env' value.
parseEnv :: RequestParser Hack.Env
parseEnv = rawEnv `fmap` ask

-- | Determine the ordered list of language preferences.
--
-- FIXME: Future versions should account for some cookie.
acceptedLanguages :: RequestParser [String]
acceptedLanguages = do
    env <- parseEnv
    let rawLang = tryLookup "" "Accept-Language" $ Hack.http env
    return $! parseHttpAccept rawLang

-- | Determinge the path requested by the user (ie, the path info).
requestPath :: RequestParser String
requestPath = do
    env <- parseEnv
    let q = case Hack.queryString env of
                "" -> ""
                q'@('?':_) -> q'
                q' -> q'
    return $! Hack.pathInfo env ++ q

type RequestParser a = WriterT [ParamError] (Reader RawRequest) a
instance Applicative (WriterT [ParamError] (Reader RawRequest)) where
    pure = return
    f <*> a = do
        f' <- f
        a' <- a
        return $! f' a'

-- | Parse a request into either the desired 'Request' or a list of errors.
runRequestParser :: RequestParser a -> RawRequest -> Either [ParamError] a
runRequestParser p req =
    let (val, errors) = (runReader (runWriterT p)) req
     in case errors of
            [] -> Right val
            x -> Left x

-- | The raw information passed through Hack, cleaned up a bit.
data RawRequest = RawRequest
    { rawPathInfo :: PathInfo
    , rawUrlParams :: [(ParamName, ParamValue)]
    , rawGetParams :: [(ParamName, ParamValue)]
    , rawPostParams :: [(ParamName, ParamValue)]
    , rawCookies :: [(ParamName, ParamValue)]
    , rawFiles :: [(ParamName, FileInfo)]
    , rawEnv :: Hack.Env
    }

-- | All GET paramater values with the given name.
getParams :: RawRequest -> ParamName -> [ParamValue]
getParams rr name = map snd
                  . filter (\x -> name == fst x)
                  . rawGetParams
                  $ rr

-- | All POST paramater values with the given name.
postParams :: RawRequest -> ParamName -> [ParamValue]
postParams rr name = map snd
                   . filter (\x -> name == fst x)
                   . rawPostParams
                   $ rr

-- | All URL paramater values (see rewriting) with the given name.
urlParams :: RawRequest -> ParamName -> [ParamValue]
urlParams rr name = map snd
                  . filter (\x -> name == fst x)
                  . rawUrlParams
                  $ rr

-- | All GET, POST and URL paramater values (see rewriting) with the given name.
anyParams :: RawRequest -> ParamName -> [ParamValue]
anyParams req name = urlParams req name ++
                     getParams req name ++
                     postParams req name

-- | All cookies with the given name.
cookies :: RawRequest -> ParamName -> [ParamValue]
cookies rr name = map snd . filter (fst `equals` name) . rawCookies $ rr

instance Parameter a => Parameter (Maybe a) where
    readParams [] = Right Nothing
    readParams [x] = readParam x >>= return . Just
    readParams xs = Left $ "Given " ++ show (length xs) ++
                           " values, expecting 0 or 1"

instance Parameter a => Parameter [a] where
    readParams = mapM readParam

instance Parameter String where
    readParam = Right

instance Parameter Int where
    readParam s = case reads s of
                    ((x, _):_) -> Right x
                    _ -> Left $ "Invalid integer: " ++ s

-- | The input for a resource.
--
-- Each resource can define its own instance of 'Request' and then more
-- easily ensure that it received the correct input (ie, correct variables,
-- properly typed).
class Request a where
    parseRequest :: RequestParser a

instance Request () where
    parseRequest = return ()

type ContentType = String

-- | The output for a resource.
class Response a where
    -- | Provide an ordered list of possible responses, depending on content
    -- type. If the user asked for a specific response type (like
    -- text/html), then that will get priority. If not, then the first
    -- element in this list will be used.
    reps :: a -> [(ContentType, Hack.Response)]

-- | Wrapper around 'Hack.Response' to allow arbitrary pieces of data to be
-- used for the body.
response :: LazyByteString lbs
         => Int
         -> [(String, String)]
         -> lbs
         -> Hack.Response
response a b c = Hack.Response a b $ toLazyByteString c

instance Response () where
    reps _ = [("text/plain", response 200 [] "")]

newtype ErrorResponse = ErrorResponse String
instance Response ErrorResponse where
    reps (ErrorResponse s) = [("text/plain", response 500 [] s)]

data ResponseWrapper = forall res. Response res => ResponseWrapper res
instance Response ResponseWrapper where
    reps (ResponseWrapper res) = reps res

-- | Contains settings and a list of resources.
type ApplicationMonad = StateT ApplicationSettings (Writer [Resource])
instance Applicative ApplicationMonad where
    pure = return
    f <*> a = do
        f' <- f
        a' <- a
        return $! f' a'
data ApplicationSettings = ApplicationSettings
    { hackHandler :: Hack.Application -> IO ()
    , rpxnowApiKey :: Maybe String
    , encryptKey :: Either FilePath Word256
    , urlRewriter :: UrlRewriter
    , hackMiddleware :: [Hack.Middleware]
    , response404 :: Hack.Env -> IO Hack.Response
    , htmlWrapper :: BS.ByteString -> BS.ByteString
    }
instance Default ApplicationSettings where
    def = ApplicationSettings
            { hackHandler = Hack.Handler.CGI.run
            , rpxnowApiKey = Nothing
            , encryptKey = Left defaultKeyFile
            , urlRewriter = \s -> (s, [])
            , hackMiddleware = [gzip, cleanPath, jsonp]
            , response404 = default404
            , htmlWrapper = id
            }

default404 :: Hack.Env -> IO Hack.Response
default404 env = return $
    Hack.Response
        404
        [("Content-Type", "text/plain")]
        $ toLazyByteString $ "Not found: " ++ Hack.pathInfo env

data Handler = forall req res. (Request req, Response res)
            => Handler (req -> IO res)
type LiftedHandler = RawRequest -> IO ResponseWrapper

liftHandler ::
               Handler
            -> RawRequest
            -> IO ResponseWrapper
liftHandler (Handler h) rr = do
    case runRequestParser parseRequest rr of
        Left errors -> return $ ResponseWrapper
                              $ ErrorResponse
                              $ unlines errors
        Right req -> ResponseWrapper `fmap` h req

type PathInfo = [String]
data Resource = Resource [Hack.RequestMethod] PathInfo LiftedHandler

-- FIXME document below here

addResource :: (Request req, Response res)
            => [Hack.RequestMethod]
            -> PathInfo
            -> (req -> IO res)
            -> ApplicationMonad ()
addResource methods path f =
    tell [Resource methods path $ liftHandler $ Handler f]

setUrlRewriter :: UrlRewriter -> ApplicationMonad ()
setUrlRewriter newUrlRewriter = do
    s <- get
    put $ s { urlRewriter = newUrlRewriter }

setHtmlWrapper :: (BS.ByteString -> BS.ByteString) -> ApplicationMonad ()
setHtmlWrapper f = do
    s <- get
    put $ s { htmlWrapper = f }

run :: ApplicationMonad () -> IO ()
run m = do
    let (settings, resources') = runWriter $ execStateT m def
    key <- case encryptKey settings of
            Left f -> getKey f
            Right k -> return k
    let defApp = defaultResources settings
        defResources = execWriter $ execStateT defApp def
        resources = resources' ++ defResources
        app' :: Hack.Application
        app' = makeApplication' resources settings
        clientsession' :: Hack.Middleware
        clientsession' = clientsession [authCookieName] key
        app :: Hack.Application
        app = foldr ($) app' $ hackMiddleware settings ++ [clientsession']
    hackHandler settings app

setHandler :: (Hack.Application -> IO ()) -> ApplicationMonad ()
setHandler h = do
    settings <- get
    put $ settings { hackHandler = h }

setRpxnowApiKey :: String -> ApplicationMonad ()
setRpxnowApiKey k = do
    settings <- get
    put $ settings { rpxnowApiKey = Just k }

defaultResources :: ApplicationSettings -> ApplicationMonad ()
defaultResources settings = do
    addResource [Hack.GET] ["auth", "check"] authCheck
    addResource [Hack.GET] ["auth", "logout"] authLogout
    addResource [Hack.GET] ["auth", "openid"] authOpenidForm
    addResource [Hack.GET] ["auth", "openid", "forward"] authOpenidForward
    addResource [Hack.GET] ["auth", "openid", "complete"] authOpenidComplete
    case rpxnowApiKey settings of
        Nothing -> return ()
        Just key -> do
            addResource [Hack.GET] ["auth", "login", "rpxnow"] $
                        rpxnowLogin key

data OIDFormReq = OIDFormReq (Maybe String) (Maybe String)
instance Request OIDFormReq where
    parseRequest = OIDFormReq <$> getParam "message" <*> getParam "dest"
instance Show OIDFormReq where
    show (OIDFormReq Nothing _) = ""
    show (OIDFormReq (Just s) _) = "<p class='message'>" ++ encodeHtml s ++
                                 "</p>"
data OIDFormRes = OIDFormRes String (Maybe String)
instance Response OIDFormRes where
    reps (OIDFormRes s dest) = [("text/html", response 200 heads s)]
        where
            heads =
                case dest of
                    Nothing -> []
                    Just dest' ->
                        [("Set-Cookie", "DEST=" ++ dest' ++ "; path=/")]
authOpenidForm :: OIDFormReq -> IO OIDFormRes
authOpenidForm m@(OIDFormReq _ dest) =
    let html =
            show m ++
            "<form method='get' action='forward/'>" ++
            "OpenID: <input type='text' name='openid'>" ++
            "<input type='submit' value='Login'>" ++
            "</form>"
     in return $! OIDFormRes html dest
data OIDFReq = OIDFReq String String
instance Request OIDFReq where
    parseRequest = do
        oid <- getParam "openid"
        env <- parseEnv
        let complete = "http://" ++ Hack.serverName env ++ ":" ++
                       show (Hack.serverPort env) ++
                       "/auth/openid/complete/"
        return $! OIDFReq oid complete
authOpenidForward :: OIDFReq -> IO GenResponse
authOpenidForward (OIDFReq oid complete) = do
    res <- OpenId.getForwardUrl oid complete :: IO (Either String String)
    return $
      case res of
        Left err -> RedirectResponse $ "/auth/openid/?message=" ++
                                       encodeUrl err
        Right url -> RedirectResponse url

data OIDComp = OIDComp [(String, String)] (Maybe String)
instance Request OIDComp where
    parseRequest = do
        rr <- ask
        let gets = rawGetParams rr
        dest <- cookieParam "DEST"
        return $! OIDComp gets dest
data OIDCompRes = OIDCompResErr String
                | OIDCompResGood String (Maybe String)
instance Response OIDCompRes where
    reps (OIDCompResErr err) =
        reps $ RedirectResponse
             $ "/auth/openid/?message=" ++
               encodeUrl err
    reps (OIDCompResGood ident Nothing) =
        reps $ OIDCompResGood ident (Just "/")
    reps (OIDCompResGood ident (Just dest)) =
        [("text/plain", response 303 heads "")] where
        heads =
            [ (authCookieName, ident)
            , resetCookie "DEST"
            , ("Location", dest)
            ]

resetCookie :: String -> (String, String)
resetCookie name =
    ("Set-Cookie",
     name ++ "=; path=/; expires=Thu, 01-Jan-1970 00:00:00 GMT")

authOpenidComplete :: OIDComp -> IO OIDCompRes
authOpenidComplete (OIDComp gets' dest) = do
    res <- OpenId.authenticate gets' :: IO (Either String OpenId.Identifier)
    return $
      case res of
        Left err -> OIDCompResErr err
        Right (OpenId.Identifier ident) -> OIDCompResGood ident dest

-- | token dest
data RpxnowRequest = RpxnowRequest String (Maybe String)
instance Request RpxnowRequest where
    parseRequest = do
        token <- getParam "token"
        dest <- getParam "dest"
        return $! RpxnowRequest token $ chopHash `fmap` dest

chopHash :: String -> String
chopHash ('#':rest) = rest
chopHash x = x

-- | dest identifier
data RpxnowResponse = RpxnowResponse String (Maybe String)
instance Response RpxnowResponse where
    reps (RpxnowResponse dest Nothing) =
        [("text/html", response 303 [("Location", dest)] "")]
    reps (RpxnowResponse dest (Just ident)) =
        [("text/html", response 303
                    [ ("Location", dest)
                    , (authCookieName, ident)
                    ]
                    "")]

rpxnowLogin :: String -- ^ api key
            -> RpxnowRequest
            -> IO RpxnowResponse
rpxnowLogin apiKey (RpxnowRequest token dest') = do
    let dest = case dest' of
                Nothing -> "/"
                Just "" -> "/"
                Just s -> s
    ident' <- Rpxnow.authenticate apiKey token
    return $ RpxnowResponse dest (Rpxnow.identifier `fmap` ident')

authCookieName :: String
authCookieName = "IDENTIFIER"

data AuthRequest = AuthRequest (Maybe String)
instance Request AuthRequest where
    parseRequest = AuthRequest `fmap` identifier

authCheck :: AuthRequest -> IO Tree
authCheck (AuthRequest Nothing) =
    return $ TreeMap [("status", TreeScalar "notloggedin")]
authCheck (AuthRequest (Just i)) =
    return $ TreeMap $
        [ ("status", TreeScalar "loggedin")
        , ("ident", TreeScalar i)
        ]

authLogout :: () -> IO LogoutResponse
authLogout _ = return LogoutResponse

data LogoutResponse = LogoutResponse
instance Response LogoutResponse where
    reps _ = map (second addCookie) $ reps tree where
        tree = TreeMap [("status", TreeScalar "loggedout")]
        addCookie (Hack.Response s h c) =
            Hack.Response s (h':h) c
        h' = resetCookie authCookieName

makeApplication' :: [Resource]
                 -> ApplicationSettings
                 -> Hack.Env
                 -> IO Hack.Response
makeApplication' resources settings env = do
    let method = Hack.requestMethod env
        rr = envToRawRequest (urlRewriter settings) env
        path' = rawPathInfo rr
        isValid :: Resource -> Bool
        isValid (Resource methods path _) = method `elem` methods
                                         && path == path'
    case filter isValid resources of
        [Resource _ _ handler] -> do
            let rawHttpAccept = tryLookup "" "Accept" $ Hack.http env
                ctypes' = parseHttpAccept rawHttpAccept
            body <- handler rr
            let reps' = reps body
                ctypes = filter (\c -> isJust $ lookup c reps') ctypes'
            let handlerPair =
                  case ctypes of
                    [] -> Just $ head reps'
                    (c:_) ->
                      case filter (fst `equals` c) reps' of
                        [pair] -> Just pair
                        [] -> Nothing
                        _ -> error "Overlapping reps"
            case handlerPair of
                Nothing -> response404 settings $ env
                Just (ctype, Hack.Response status headers content) -> do
                    let wrapper =
                            case ctype of
                                "text/html" -> htmlWrapper settings
                                _ -> id
                    return $ Hack.Response status
                             (("Content-Type", ctype) : headers)
                           $ toLazyByteString $ wrapper content
        [] -> response404 settings $ env
        _ -> fail "Overlapping handlers"

type UrlRewriter = PathInfo -> (PathInfo, [(String, String)])
envToRawRequest :: UrlRewriter -> Hack.Env -> RawRequest
envToRawRequest rewriter env =
    let (Right rawPieces) = splitPath $ Hack.pathInfo env
        (pi', urls) = rewriter rawPieces
        gets' = decodeUrlPairs $ Hack.queryString env :: [(String, String)]
        clength = tryLookup "0" "Content-Length" $ Hack.http env
        ctype = tryLookup "" "Content-Type" $ Hack.http env
        (posts, files) = parsePost ctype clength
                       $ Hack.hackInput env
        rawCookie = tryLookup "" "Cookie" $ Hack.http env
        cookies' = decodeCookies rawCookie :: [(String, String)]
     in RawRequest pi' urls gets' posts cookies' files env

data Tree = TreeScalar String
          | TreeList [Tree]
          | TreeMap [(String, Tree)]
class IsTree a where
    toTree :: a -> Tree

treeToJson :: Tree -> String
treeToJson (TreeScalar s) = '"' : encodeJson s ++ "\""
treeToJson (TreeList l) =
    "[" ++ intercalate "," (map treeToJson l) ++ "]"
treeToJson (TreeMap m) =
    "{" ++ intercalate "," (map helper m) ++ "}" where
        helper (k, v) =
            treeToJson (TreeScalar k) ++
            ":" ++
            treeToJson v

treeToHtml :: Tree -> String
treeToHtml (TreeScalar s) = encodeHtml s
treeToHtml (TreeList l) =
    "<ul>" ++ concatMap (\e -> "<li>" ++ treeToHtml e ++ "</li>") l ++
    "</ul>"
treeToHtml (TreeMap m) =
    "<dl>" ++
    concatMap (\(k, v) -> "<dt>" ++ encodeHtml k ++ "</dt>" ++
                          "<dd>" ++ treeToHtml v ++ "</dd>") m ++
    "</dl>"

instance Response Tree where
    reps tree =
        [ ("text/html", response 200 [] $ treeToHtml tree)
        , ("application/json", response 200 [] $ treeToJson tree)
        ]

parseHttpAccept :: String -> [String]
parseHttpAccept = filter (not . specialHttpAccept) . splitOneOf ";,"

specialHttpAccept :: String -> Bool
specialHttpAccept ('q':'=':_) = True
specialHttpAccept ('*':_) = True
specialHttpAccept _ = False

data AtomFeed = AtomFeed
    { atomTitle :: String
    , atomLinkSelf :: String
    , atomLinkHome :: String
    , atomUpdated :: UTCTime
    , atomEntries :: [AtomFeedEntry]
    }
instance Response AtomFeed where
    reps e =
        [ ("application/atom+xml", response 200 [] $ show e)
        ]

data AtomFeedEntry = AtomFeedEntry
    { atomEntryLink :: String
    , atomEntryUpdated :: UTCTime
    , atomEntryTitle :: String
    , atomEntryContent :: String
    }

instance Show AtomFeed where
    show f = concat
        [ "<?xml version='1.0' encoding='utf-8' ?>\n"
        , "<feed xmlns='http://www.w3.org/2005/Atom'>"
        , "<title>"
        , encodeHtml $ atomTitle f
        , "</title>"
        , "<link rel='self' href='"
        , encodeHtml $ atomLinkSelf f
        , "'/>"
        , "<link href='"
        , encodeHtml $ atomLinkHome f
        , "'/>"
        , "<updated>"
        , formatW3 $ atomUpdated f
        , "</updated>"
        , "<id>"
        , encodeHtml $ atomLinkHome f
        , "</id>"
        , concatMap show $ atomEntries f
        , "</feed>"
        ]

instance Show AtomFeedEntry where
    show e = concat
        [ "<entry>"
        , "<id>"
        , encodeHtml $ atomEntryLink e
        , "</id>"
        , "<link href='"
        , encodeHtml $ atomEntryLink e
        , "' />"
        , "<updated>"
        , formatW3 $ atomEntryUpdated e
        , "</updated>"
        , "<title>"
        , encodeHtml $ atomEntryTitle e
        , "</title>"
        , "<content type='html'><![CDATA["
        , atomEntryContent e
        , "]]></content>"
        , "</entry>"
        ]

formatW3 :: UTCTime -> String
formatW3 = formatTime defaultTimeLocale "%FT%X-08:00"

class IsTree a => ListDetail a where
    htmlDetail :: a -> String
    htmlDetail = treeToHtml . toTree
    detailTitle :: a -> String
    detailUrl :: a -> String
    htmlList :: [a] -> String
    htmlList l = "<ul>" ++ concatMap helper l ++ "</ul>"
        where
            helper i = "<li><a href=\"" ++ encodeHtml (detailUrl i) ++
                       "\">" ++ encodeHtml (detailTitle i) ++
                       "</a></li>"
    -- | Often times for the JSON response of the list, we don't need all
    -- the information.
    treeList :: [a] -> Tree
    treeList = TreeList . map treeListSingle
    treeListSingle :: a -> Tree
    treeListSingle = toTree

newtype ItemList a = ItemList [a]
instance ListDetail a => Response (ItemList a) where
    reps (ItemList l) =
        [ ("text/html", response 200 [] $ htmlList l)
        , ("application/json", response 200 [] $ treeToJson $ treeList l)
        ]
newtype ItemDetail a = ItemDetail a
instance ListDetail a => Response (ItemDetail a) where
    reps (ItemDetail i) =
        [ ("text/html", response 200 [] $ htmlDetail i)
        , ("application/json", response 200 [] $ treeToJson $ toTree i)
        ]

-- sitemaps
data SitemapLoc = AbsLoc String | RelLoc String
data SitemapChangeFreq = Always
                       | Hourly
                       | Daily
                       | Weekly
                       | Monthly
                       | Yearly
                       | Never
instance Show SitemapChangeFreq where
    show Always = "always"
    show Hourly = "hourly"
    show Daily = "daily"
    show Weekly = "weekly"
    show Monthly = "monthly"
    show Yearly = "yearly"
    show Never = "never"

data SitemapUrl = SitemapUrl
    { sitemapLoc :: SitemapLoc
    , sitemapLastMod :: UTCTime
    , sitemapChangeFreq :: SitemapChangeFreq
    , priority :: Double
    }
data SitemapRequest = SitemapRequest String Int
instance Request SitemapRequest where
    parseRequest = do
        env <- parseEnv
        return $! SitemapRequest (Hack.serverName env)
                                 (Hack.serverPort env)
data SitemapResponse = SitemapResponse SitemapRequest [SitemapUrl]
instance Show SitemapResponse where
    show (SitemapResponse (SitemapRequest host port) urls) =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
        "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">" ++
        concatMap helper urls ++
        "</urlset>"
        where
            prefix = "http://" ++ host ++
                        case port of
                            80 -> ""
                            _ -> ":" ++ show port
            helper (SitemapUrl loc modTime freq pri) = concat
                [ "<url><loc>"
                , encodeHtml $ showLoc loc
                , "</loc><lastmod>"
                , formatW3 modTime
                , "</lastmod><changefreq>"
                , show freq
                , "</changefreq><priority>"
                , show pri
                , "</priority></url>"
                ]
            showLoc (AbsLoc s) = s
            showLoc (RelLoc s) = prefix ++ s

instance Response SitemapResponse where
    reps res =
        [ ("text/xml", response 200 [] $ show res)
        ]

sitemap :: IO [SitemapUrl] -> SitemapRequest -> IO SitemapResponse
sitemap urls' req = do
    urls <- urls'
    return $ SitemapResponse req urls

-- misc helper functions
tryLookup :: Eq k => v -> k -> [(k, v)] -> v
tryLookup v _ [] = v
tryLookup v k ((k', v'):rest)
    | k == k' = v'
    | otherwise = tryLookup v k rest

data GenResponse = HtmlResponse String
                 | TreeResponse Tree
                 | HtmlOrTreeResponse String Tree
                 | RedirectResponse String
                 | PermissionDeniedResult String
                 | NotFoundResponse String
instance Response GenResponse where
    reps (HtmlResponse h) = [("text/html", response 200 [] h)]
    reps (TreeResponse t) = reps t
    reps (HtmlOrTreeResponse h t) =
        ("text/html", response 200 [] h) : reps t
    reps (RedirectResponse url) = [("text/html", response 303 heads body)]
      where
        heads = [("Location", url)]
        body = "<p>Redirecting to <a href='" ++ encodeHtml url ++
               "'>" ++ encodeHtml url ++ "</a></p>"
    reps (PermissionDeniedResult s) = [("text/plain", response 403 [] s)]
    reps (NotFoundResponse s) = [("text/plain", response 404 [] s)]
