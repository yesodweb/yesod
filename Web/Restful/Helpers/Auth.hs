{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
---------------------------------------------------------
--
-- Module        : Web.Restful.Helpers.Auth
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Authentication through the authentication package.
--
---------------------------------------------------------
module Web.Restful.Helpers.Auth
    ( AuthResource
    , FromAuthResource (..)
    , authResourceParser
    ) where

import qualified Hack
import Web.Encodings
import qualified Web.Authenticate.Rpxnow as Rpxnow
import qualified Web.Authenticate.OpenId as OpenId

import Web.Restful
import Web.Restful.Constants

import Control.Applicative ((<$>), Applicative (..))
import Control.Arrow (second)
import Control.Monad.Reader

import Data.Object

data AuthResource =
    AuthCheck
    | AuthLogout
    | AuthOpenid
    | AuthOpenidForward
    | AuthOpenidComplete
    | AuthLoginRpxnow
    deriving Eq
class FromAuthResource a where
    fromAuthResource :: AuthResource -> a

authResourceParser :: FromAuthResource far
                   => Resource
                   -> Maybe (ParsedResource far)
authResourceParser ["check"] =
    authResourceParser' AuthCheck
authResourceParser ["logout"] =
    authResourceParser' AuthLogout
authResourceParser ["openid"] =
    authResourceParser' AuthOpenid
authResourceParser ["openid", "forward"] =
    authResourceParser' AuthOpenidForward
authResourceParser ["openid", "complete"] =
    authResourceParser' AuthOpenidComplete
authResourceParser ["login", "rpxnow"] =
    authResourceParser' AuthLoginRpxnow
authResourceParser _ = Nothing

authResourceParser' :: FromAuthResource far
                    => AuthResource
                    -> Maybe (ParsedResource far)
authResourceParser' x = Just $ ParsedResource (fromAuthResource x) []

type RpxnowApiKey = String -- FIXME newtype
instance HasHandlers AuthResource (Maybe RpxnowApiKey) where
    getHandler _ AuthCheck Get = liftHandler authCheck
    getHandler _ AuthLogout Get = liftHandler authLogout
    getHandler _ AuthOpenid Get = liftHandler authOpenidForm
    getHandler _ AuthOpenidForward Get = liftHandler authOpenidForward
    getHandler _ AuthOpenidComplete Get = liftHandler authOpenidComplete
    getHandler (Just key) AuthLoginRpxnow Get = liftHandler $ rpxnowLogin key
    getHandler _ _ _ = Nothing

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
