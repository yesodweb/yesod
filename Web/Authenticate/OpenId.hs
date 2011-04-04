{-# LANGUAGE FlexibleContexts #-}
module Web.Authenticate.OpenId
    ( getForwardUrl
    , authenticate
    , AuthenticateException (..)
    , Identifier (..)
    ) where

import Control.Monad.IO.Class
import OpenId2.Normalization (normalize)
import OpenId2.Discovery (discover, Discovery (..))
import Control.Failure (Failure (failure))
import OpenId2.Types
import Web.Authenticate.Internal (qsUrl)
import Control.Monad (unless)
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Network.HTTP.Enumerator
    ( parseUrl, urlEncodedBody, responseBody, httpLbsRedirect
    , HttpException, withManager
    )
import Control.Arrow ((***))
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)

getForwardUrl
    :: ( MonadIO m
       , Failure AuthenticateException m
       , Failure HttpException m
       )
    => String -- ^ The openid the user provided.
    -> String -- ^ The URL for this application\'s complete page.
    -> Maybe String -- ^ Optional realm
    -> [(String, String)] -- ^ Additional parameters to send to the OpenID provider. These can be useful for using extensions.
    -> m String -- ^ URL to send the user to.
getForwardUrl openid' complete mrealm params = do
    let realm = fromMaybe complete mrealm
    disc <- normalize openid' >>= discover
    case disc of
        Discovery1 server mdelegate ->
            return $ qsUrl server
                $ ("openid.mode", "checkid_setup")
                : ("openid.identity", fromMaybe openid' mdelegate)
                : ("openid.return_to", complete)
                : ("openid.realm", realm)
                : ("openid.trust_root", complete)
                : params
        Discovery2 (Provider p) (Identifier i) itype -> do
            let i' =
                    case itype of
                        ClaimedIdent -> i
                        OPIdent -> "http://specs.openid.net/auth/2.0/identifier_select"
            return $ qsUrl p
                $ ("openid.ns", "http://specs.openid.net/auth/2.0")
                : ("openid.mode", "checkid_setup")
                : ("openid.claimed_id", i')
                : ("openid.identity", i')
                : ("openid.return_to", complete)
                : ("openid.realm", realm)
                : params

authenticate
    :: ( MonadIO m
       , Failure AuthenticateException m
       , Failure HttpException m
       )
    => [(String, String)]
    -> m (Identifier, [(String, String)])
authenticate params = do
    unless (lookup "openid.mode" params == Just "id_res")
        $ failure $ case lookup "openid.mode" params of
                      Nothing -> AuthenticationException "openid.mode was not found in the params."
                      (Just m)
                            | m == "error" ->
                                case lookup "openid.error" params of
                                  Nothing -> AuthenticationException "An error occurred, but no error message was provided."
                                  (Just e) -> AuthenticationException e
                            | otherwise -> AuthenticationException $ "mode is " ++ m ++ " but we were expecting id_res."
    ident <- case lookup "openid.identity" params of
                Just i -> return i
                Nothing ->
                    failure $ AuthenticationException "Missing identity"
    disc <- normalize ident >>= discover
    let endpoint = case disc of
                    Discovery1 p _ -> p
                    Discovery2 (Provider p) _ _ -> p
    let params' = map (BSU.fromString *** BSU.fromString)
                $ ("openid.mode", "check_authentication")
                : filter (\(k, _) -> k /= "openid.mode") params
    req' <- parseUrl endpoint
    let req = urlEncodedBody params' req'
    rsp <- liftIO $ withManager $ httpLbsRedirect req
    let rps = parseDirectResponse $ BSLU.toString $ responseBody rsp
    case lookup "is_valid" rps of
        Just "true" -> return (Identifier ident, rps)
        _ -> failure $ AuthenticationException "OpenID provider did not validate"

-- | Turn a response body into a list of parameters.
parseDirectResponse :: String -> [(String, String)]
parseDirectResponse  = unfoldr step
  where
    step []  = Nothing
    step str = case split (== '\n') str of
      (ps,rest) -> Just (split (== ':') ps,rest)

split :: (a -> Bool) -> [a] -> ([a],[a])
split p as = case break p as of
  (xs,_:ys) -> (xs,ys)
  pair      -> pair
