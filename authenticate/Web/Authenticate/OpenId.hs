{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Authenticate.OpenId
    ( getForwardUrl
    , authenticate
    , AuthenticateException (..)
    , Identifier (..)
    ) where

import Control.Monad.IO.Class
import OpenId2.Normalization (normalize)
import OpenId2.Discovery (discover, Discovery (..))
import OpenId2.Types
import Control.Monad (unless)
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy (toStrict)
import Network.HTTP.Conduit
    ( parseUrl, urlEncodedBody, responseBody, httpLbs
    , Manager
    )
import Data.Conduit (ResourceT, ResourceIO)
import Control.Arrow ((***), second)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Blaze.ByteString.Builder (toByteString)
import Network.HTTP.Types (renderQueryText)
import Data.Monoid (mappend)
import Control.Exception (throwIO)

getForwardUrl
    :: ResourceIO m
    => Text -- ^ The openid the user provided.
    -> Text -- ^ The URL for this application\'s complete page.
    -> Maybe Text -- ^ Optional realm
    -> [(Text, Text)] -- ^ Additional parameters to send to the OpenID provider. These can be useful for using extensions.
    -> Manager
    -> ResourceT m Text -- ^ URL to send the user to.
getForwardUrl openid' complete mrealm params manager = do
    let realm = fromMaybe complete mrealm
    disc <- normalize openid' >>= flip discover manager
    let helper s q = return $ T.concat
            [ s
            , if "?" `T.isInfixOf` s then "&" else "?"
            , decodeUtf8 (toByteString $ renderQueryText False $ map (second Just) q)
            ]
    case disc of
        Discovery1 server mdelegate -> helper server
                $ ("openid.mode", "checkid_setup")
                : ("openid.identity", maybe openid' id mdelegate)
                : ("openid.return_to", complete)
                : ("openid.realm", realm)
                : ("openid.trust_root", complete)
                : params
        Discovery2 (Provider p) (Identifier i) itype -> do
            let i' =
                    case itype of
                        ClaimedIdent -> i
                        OPIdent -> "http://specs.openid.net/auth/2.0/identifier_select"
            helper p
                $ ("openid.ns", "http://specs.openid.net/auth/2.0")
                : ("openid.mode", "checkid_setup")
                : ("openid.claimed_id", i')
                : ("openid.identity", i')
                : ("openid.return_to", complete)
                : ("openid.realm", realm)
                : params

authenticate
    :: ResourceIO m
    => [(Text, Text)]
    -> Manager
    -> ResourceT m (Identifier, [(Text, Text)])
authenticate params manager = do
    unless (lookup "openid.mode" params == Just "id_res")
        $ liftIO $ throwIO $ case lookup "openid.mode" params of
                      Nothing -> AuthenticationException "openid.mode was not found in the params."
                      (Just m)
                            | m == "error" ->
                                case lookup "openid.error" params of
                                  Nothing -> AuthenticationException "An error occurred, but no error message was provided."
                                  (Just e) -> AuthenticationException $ unpack e
                            | otherwise -> AuthenticationException $ "mode is " ++ unpack m ++ " but we were expecting id_res."
    ident <- case lookup "openid.identity" params of
                Just i -> return i
                Nothing ->
                    liftIO $ throwIO $ AuthenticationException "Missing identity"
    disc <- normalize ident >>= flip discover manager
    let endpoint = case disc of
                    Discovery1 p _ -> p
                    Discovery2 (Provider p) _ _ -> p
    let params' = map (encodeUtf8 *** encodeUtf8)
                $ ("openid.mode", "check_authentication")
                : filter (\(k, _) -> k /= "openid.mode") params
    req' <- liftIO $ parseUrl $ unpack endpoint
    let req = urlEncodedBody params' req'
    rsp <- httpLbs req manager
    let rps = parseDirectResponse $ toStrict $ decodeUtf8With lenientDecode $ responseBody rsp
    case lookup "is_valid" rps of
        Just "true" -> return (Identifier ident, rps)
        _ -> liftIO $ throwIO $ AuthenticationException "OpenID provider did not validate"

-- | Turn a response body into a list of parameters.
parseDirectResponse :: Text -> [(Text, Text)]
parseDirectResponse  =
    map (pack *** pack) . unfoldr step . unpack
  where
    step []  = Nothing
    step str = case split (== '\n') str of
      (ps,rest) -> Just (split (== ':') ps,rest)

split :: (a -> Bool) -> [a] -> ([a],[a])
split p as = case break p as of
  (xs,_:ys) -> (xs,ys)
  pair      -> pair
