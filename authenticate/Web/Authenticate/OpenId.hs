{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Authenticate.OpenId
    ( -- * Functions
      getForwardUrl
    , authenticate
    , authenticateClaimed
      -- * Types
    , AuthenticateException (..)
    , Identifier (..)
      -- ** Response
    , OpenIdResponse
    , oirOpLocal
    , oirParams
    , oirClaimed
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
import Control.Arrow ((***), second)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Blaze.ByteString.Builder (toByteString)
import Network.HTTP.Types (renderQueryText)
import Control.Exception (throwIO)
import Data.Conduit (MonadBaseControl, MonadResource)

getForwardUrl
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ The openid the user provided.
    -> Text -- ^ The URL for this application\'s complete page.
    -> Maybe Text -- ^ Optional realm
    -> [(Text, Text)] -- ^ Additional parameters to send to the OpenID provider. These can be useful for using extensions.
    -> Manager
    -> m Text -- ^ URL to send the user to.
getForwardUrl openid' complete mrealm params manager = do
    let realm = fromMaybe complete mrealm
    claimed <- normalize openid'
    disc <- discover claimed manager
    let helper s q = return $ T.concat
            [ s
            , if "?" `T.isInfixOf` s then "&" else "?"
            , decodeUtf8 (toByteString $ renderQueryText False $ map (second Just) q)
            ]
    case disc of
        Discovery1 server mdelegate -> helper server
                $ ("openid.mode", "checkid_setup")
                : ("openid.identity", maybe (identifier claimed) id mdelegate)
                : ("openid.return_to", complete)
                : ("openid.realm", realm)
                : ("openid.trust_root", complete)
                : params
        Discovery2 (Provider p) (Identifier i) itype -> do
            let (claimed', identity') =
                    case itype of
                        ClaimedIdent -> (identifier claimed, i)
                        OPIdent ->
                            let x = "http://specs.openid.net/auth/2.0/identifier_select"
                             in (x, x)
            helper p
                $ ("openid.ns", "http://specs.openid.net/auth/2.0")
                : ("openid.mode", "checkid_setup")
                : ("openid.claimed_id", claimed')
                : ("openid.identity", identity')
                : ("openid.return_to", complete)
                : ("openid.realm", realm)
                : params

authenticate
    :: (MonadBaseControl IO m, MonadResource m, MonadIO m)
    => [(Text, Text)]
    -> Manager
    -> m (Identifier, [(Text, Text)])
authenticate ps m = do
    x <- authenticateClaimed ps m
    return (oirOpLocal x, oirParams x)
{-# DEPRECATED authenticate "Use authenticateClaimed" #-}

data OpenIdResponse = OpenIdResponse
    { oirOpLocal :: Identifier
    , oirParams :: [(Text, Text)]
    , oirClaimed :: Maybe Identifier
    }

authenticateClaimed
    :: (MonadBaseControl IO m, MonadResource m, MonadIO m)
    => [(Text, Text)]
    -> Manager
    -> m OpenIdResponse
authenticateClaimed params manager = do
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
    discOP <- normalize ident >>= flip discover manager

    let endpoint d =
            case d of
                Discovery1 p _ -> p
                Discovery2 (Provider p) _ _ -> p
    let params' = map (encodeUtf8 *** encodeUtf8)
                $ ("openid.mode", "check_authentication")
                : filter (\(k, _) -> k /= "openid.mode") params
    req' <- liftIO $ parseUrl $ unpack $ endpoint discOP
    let req = urlEncodedBody params' req'
    rsp <- httpLbs req manager
    let rps = parseDirectResponse $ toStrict $ decodeUtf8With lenientDecode $ responseBody rsp

    claimed <-
        case lookup "openid.claimed_id" params of
            Nothing -> return Nothing
            Just claimed' -> do
                -- need to validate that this provider can speak for the given
                -- claimed identifier
                claimedN <- normalize claimed'
                discC <- discover claimedN manager
                return $
                    if endpoint discOP == endpoint discC
                        then Just claimedN
                        else Nothing

    case lookup "is_valid" rps of
        Just "true" -> return OpenIdResponse
            { oirOpLocal = Identifier ident
            , oirParams  = rps
            , oirClaimed = claimed
            }
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
