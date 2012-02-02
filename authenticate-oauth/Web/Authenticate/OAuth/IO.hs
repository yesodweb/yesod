{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-- | This Module provides interface for the instance of ResouceIO instead of ResourceT.
-- What this module do is just adding 'withManager' or 'runResourceT'.
module Web.Authenticate.OAuth.IO
    ( 
      module Web.Authenticate.OAuth,
      getAccessToken, signOAuth,
      getTemporaryCredential, getTemporaryCredentialWithScope,
      getTemporaryCredentialProxy, getTemporaryCredential',
      getTokenCredential,
      getAccessTokenProxy, getTokenCredentialProxy, 
      getAccessToken', genSign
    ) where
import Network.HTTP.Conduit
import qualified Web.Authenticate.OAuth as OA
import Web.Authenticate.OAuth hiding
    (getAccessToken, signOAuth,
     getTemporaryCredential, getTemporaryCredentialWithScope,
     getTemporaryCredentialProxy, getTemporaryCredential',
     getTokenCredential, getTemporaryCredentialWithScope,
     getAccessTokenProxy, getTemporaryCredentialProxy,
     getTokenCredentialProxy, genSign,
     getAccessToken', getTemporaryCredential')
import Data.Conduit
import qualified Data.ByteString.Char8 as BS


-- | Get temporary credential for requesting acces token.
getTemporaryCredential :: ResourceIO m
                       => OA.OAuth         -- ^ OAuth Application
                       -> m OA.Credential -- ^ Temporary Credential (Request Token & Secret).
getTemporaryCredential = withManager . OA.getTemporaryCredential

-- | Get temporary credential for requesting access token with Scope parameter.
getTemporaryCredentialWithScope :: ResourceIO m
                                => BS.ByteString -- ^ Scope parameter string
                                -> OAuth         -- ^ OAuth Application
                                -> m Credential -- ^ Temporay Credential (Request Token & Secret).
getTemporaryCredentialWithScope bs oa =
    withManager $ OA.getTemporaryCredentialWithScope bs oa


-- | Get temporary credential for requesting access token via the proxy.
getTemporaryCredentialProxy :: ResourceIO m
                            => Maybe Proxy   -- ^ Proxy
                            -> OAuth         -- ^ OAuth Application
                            -> m Credential -- ^ Temporary Credential (Request Token & Secret).
getTemporaryCredentialProxy p oa = withManager $ OA.getTemporaryCredential' (addMaybeProxy p) oa

getTemporaryCredential' :: ResourceIO m
                        => (Request m -> Request m)   -- ^ Request Hook
                        -> OAuth                      -- ^ OAuth Application
                        -> m Credential -- ^ Temporary Credential (Request Token & Secret).
getTemporaryCredential' hook oa = withManager $ OA.getTemporaryCredential' hook oa


-- | Get Access token.
getAccessToken, getTokenCredential
               :: ResourceIO m
               => OAuth         -- ^ OAuth Application
               -> Credential    -- ^ Temporary Credential with oauth_verifier
               -> m Credential -- ^ Token Credential (Access Token & Secret)
getAccessToken oa cr = withManager $ OA.getAccessToken oa cr

-- | Get Access token via the proxy.
getAccessTokenProxy, getTokenCredentialProxy
               :: ResourceIO m
               => Maybe Proxy   -- ^ Proxy
               -> OAuth         -- ^ OAuth Application
               -> Credential    -- ^ Temporary Credential with oauth_verifier
               -> m Credential -- ^ Token Credential (Access Token & Secret)
getAccessTokenProxy p oa cr = withManager $ OA.getAccessTokenProxy p oa cr

getAccessToken' :: ResourceIO m
                => (Request m -> Request m)   -- ^ Request Hook
                -> OAuth                      -- ^ OAuth Application
                -> Credential                 -- ^ Temporary Credential with oauth_verifier
                -> m Credential     -- ^ Token Credential (Access Token & Secret)
getAccessToken' hook oa cr = withManager $ OA.getAccessToken' hook oa cr


getTokenCredential = getAccessToken
getTokenCredentialProxy = getAccessTokenProxy

-- | Add OAuth headers & sign to 'Request'
signOAuth :: ResourceIO m
          => OAuth              -- ^ OAuth Application
          -> Credential         -- ^ Credential
          -> Request m          -- ^ Original Request
          -> m (Request m)    -- ^ Signed OAuth Request
signOAuth oa crd req = runResourceT $ OA.signOAuth oa crd req

genSign :: ResourceIO m => OAuth -> Credential -> Request m -> m BS.ByteString
genSign oa tok req = runResourceT $ OA.genSign oa tok req
