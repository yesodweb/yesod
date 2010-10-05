{-# LANGUAGE FlexibleContexts #-}
module Web.Authenticate.OpenId2
    ( getForwardUrl
    , authenticate
    , OpenIdException (..)
    ) where

import Control.Monad.IO.Class
import OpenId2.Normalization (normalize)
import OpenId2.Discovery (discover)
import OpenId2.HTTP (makeRequest, parseDirectResponse)
import Control.Failure (Failure (failure))
import OpenId2.Types (OpenIdException (..), Identifier (Identifier),
                      Provider (Provider))
import Web.Authenticate.Internal (qsUrl)
import Control.Monad (unless)
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Network.HTTP.Enumerator (parseUrl, urlEncodedBody, responseBody)
import Control.Arrow ((***))

getForwardUrl :: (MonadIO m, Failure OpenIdException m)
              => String -- ^ The openid the user provided.
              -> String -- ^ The URL for this application\'s complete page.
              -> m String -- ^ URL to send the user to.
getForwardUrl openid' complete = do
    let resolve = makeRequest True
    (Provider p, Identifier i) <- normalize openid' >>= discover resolve
    return $ qsUrl (show p)
        [ ("openid.ns", "http://specs.openid.net/auth/2.0")
        , ("openid.mode", "checkid_setup")
        , ("openid.claimed_id", i)
        , ("openid.identity", i)
        , ("openid.return_to", complete)
        ]

authenticate :: (MonadIO m, Failure OpenIdException m)
             => [(String, String)]
             -> m String
authenticate params = do
    unless (lookup "openid.mode" params == Just "id_res")
        $ failure $ AuthenticationException "mode is not id_res"
    ident <- case lookup "openid.identity" params of
                Just i -> return i
                Nothing ->
                    failure $ AuthenticationException "Missing identity"
    endpoint <-
        case lookup "openid.op_endpoint" params of
            Just e -> return e
            Nothing ->
                failure $ AuthenticationException "Missing op_endpoint"
    let params' = map (BSU.fromString *** BSU.fromString)
                $ ("openid.mode", "check_authentication")
                : filter (\(k, _) -> k /= "openid.mode") params
    req' <- liftIO $ parseUrl endpoint
    let req = urlEncodedBody params' req'
    rsp <- liftIO $ makeRequest True req
    let rps = parseDirectResponse $ BSLU.toString $ responseBody rsp
    case lookup "is_valid" rps of
        Just "true" -> return ident
        Nothing ->
            failure $ AuthenticationException "OpenID provider did not validate"
    -- FIXME check if endpoint is valid for given identity
