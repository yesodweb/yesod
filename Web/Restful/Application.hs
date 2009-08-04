{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
---------------------------------------------------------
--
-- Module        : Web.Restful.Application
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Defining the application.
--
---------------------------------------------------------
module Web.Restful.Application
    (
      -- * Defining an application
      ApplicationMonad
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
import Web.Encodings
import Data.Maybe (isJust)
import Data.ByteString.Class
import qualified Data.ByteString.Lazy as BS
import Data.Function.Predicate (equals)
import Data.Default
import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId

import Hack.Middleware.Gzip
import Hack.Middleware.CleanPath
import Hack.Middleware.Jsonp
import Hack.Middleware.ClientSession
import Hack.Middleware.MethodOverride

import Control.Applicative ((<$>), Applicative (..))
import Control.Arrow (second)

import Web.Restful.Request
import Web.Restful.Response
import Web.Restful.Constants
import Web.Restful.Utils
import Data.Object

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
            , hackMiddleware =
                [ gzip
                , cleanPath
                , jsonp
                , methodOverride
                ]
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

data AuthRequest = AuthRequest (Maybe String)
instance Request AuthRequest where
    parseRequest = AuthRequest `fmap` identifier

authCheck :: AuthRequest -> IO Object
authCheck (AuthRequest Nothing) =
    return $ toObject [("status", "notloggedin")]
authCheck (AuthRequest (Just i)) =
    return $ toObject
        [ ("status", "loggedin")
        , ("ident", i)
        ]

authLogout :: () -> IO LogoutResponse
authLogout _ = return LogoutResponse

data LogoutResponse = LogoutResponse
instance Response LogoutResponse where
    reps _ = map (second addCookie) $ reps tree where
        tree = toObject [("status", "loggedout")]
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
