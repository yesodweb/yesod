{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , setResourceParser
    , setHtmlWrapper
      -- ** Engage
    , run
     -- * FIXME
    , Application (..)
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
import Web.Restful.Handler
import Web.Restful.Definitions
import Data.Object

-- | Contains settings and a list of resources.
type ApplicationMonad a = StateT (ApplicationSettings a) (Writer (HandlerMap a))
instance Applicative (ApplicationMonad a) where
    pure = return
    f <*> a = do
        f' <- f
        a' <- a
        return $! f' a'
data ApplicationSettings rn = ApplicationSettings
    { hackHandler :: Hack.Application -> IO ()
    , rpxnowApiKey :: Maybe String
    , encryptKey :: Either FilePath Word256
    , appResourceParser :: ResourceParser rn
    , hackMiddleware :: [Hack.Middleware]
    , response404 :: Hack.Env -> IO Hack.Response
    , htmlWrapper :: BS.ByteString -> BS.ByteString
    }
instance ResourceName a => Default (ApplicationSettings a) where
    def = ApplicationSettings
            { hackHandler = Hack.Handler.CGI.run
            , rpxnowApiKey = Nothing
            , encryptKey = Left defaultKeyFile
            , appResourceParser = \s -> ParsedResource (toResourceName s) []
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

-- FIXME document below here

addResource :: (Request req, Response res, ResourceName rn)
            => Verb
            -> rn
            -> (req -> IO res)
            -> ApplicationMonad rn ()
addResource verb resourceName' f = do
    let handler :: Handler
        handler = Handler $ (fmap ResponseWrapper) . f
        handlerDesc = HandlerDesc resourceName' verb handler
    tell [handlerDesc]

setResourceParser :: ResourceName rn
                  => ResourceParser rn
                  -> ApplicationMonad rn ()
setResourceParser newRP = do
    s <- get
    put $ s { appResourceParser = newRP }

setHtmlWrapper :: (BS.ByteString -> BS.ByteString) -> ApplicationMonad a ()
setHtmlWrapper f = do
    s <- get
    put $ s { htmlWrapper = f }

run :: ResourceName a => ApplicationMonad a () -> IO ()
run m = do
    let (settings, resources') = runWriter $ execStateT m def
    key <- case encryptKey settings of
            Left f -> getKey f
            Right k -> return k
    let defApp = defaultResources settings
        defResources = execWriter $ execStateT defApp def
        resources = resources' ++ defResources -- FIXME rename HandlerDescs
        app' :: Hack.Application
        app' = toHackApplication $ Application resources settings
        clientsession' :: Hack.Middleware
        clientsession' = clientsession [authCookieName] key
        app :: Hack.Application
        app = foldr ($) app' $ hackMiddleware settings ++ [clientsession']
    hackHandler settings app

setHandler :: (Hack.Application -> IO ()) -> ApplicationMonad a ()
setHandler h = do
    settings <- get
    put $ settings { hackHandler = h }

setRpxnowApiKey :: String -> ApplicationMonad a ()
setRpxnowApiKey k = do
    settings <- get
    put $ settings { rpxnowApiKey = Just k }

defaultResources :: ResourceName rn
                 => ApplicationSettings rn
                 -> ApplicationMonad rn ()
defaultResources settings = do
    addResource Get (toResourceName ["auth", "check"]) authCheck
    addResource Get (toResourceName ["auth", "logout"]) authLogout
    addResource Get (toResourceName ["auth", "openid"]) authOpenidForm
    addResource Get (toResourceName ["auth", "openid", "forward"]) authOpenidForward
    addResource Get (toResourceName ["auth", "openid", "complete"]) authOpenidComplete
    case rpxnowApiKey settings of
        Nothing -> return ()
        Just key -> do
            addResource Get (toResourceName ["auth", "login", "rpxnow"]) $
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

toHackApplication :: Eq resourceName
                  => Application resourceName
                  -> Hack.Application
toHackApplication (Application hm settings) env = do
    let (Right resource) = splitPath $ Hack.pathInfo env
        (ParsedResource rn urlParams') = (appResourceParser settings) resource
        verb :: Verb
        verb = toVerb $ Hack.requestMethod env
        rr :: RawRequest
        rr = envToRawRequest urlParams' env
        matchingHandler (HandlerDesc resourceName' verb' _) =
            rn == resourceName' &&
            verb == verb'
    case filter matchingHandler hm of
        [HandlerDesc _ _ handler] -> do
            let rawHttpAccept = tryLookup "" "Accept" $ Hack.http env
                ctypes' = parseHttpAccept rawHttpAccept
            body <- runHandler handler rr
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
        _ -> fail $ "Overlapping handlers for: " ++ show env

envToRawRequest :: [(ParamName, ParamValue)] -> Hack.Env -> RawRequest
envToRawRequest urlParams' env =
    let (Right rawPieces) = splitPath $ Hack.pathInfo env
        gets' = decodeUrlPairs $ Hack.queryString env :: [(String, String)]
        clength = tryLookup "0" "Content-Length" $ Hack.http env
        ctype = tryLookup "" "Content-Type" $ Hack.http env
        (posts, files) = parsePost ctype clength
                       $ Hack.hackInput env
        rawCookie = tryLookup "" "Cookie" $ Hack.http env
        cookies' = decodeCookies rawCookie :: [(String, String)]
     in RawRequest rawPieces urlParams' gets' posts cookies' files env

data Application a = Application
    { handlerMap :: HandlerMap a
    , applicationSettings :: ApplicationSettings a
    }
