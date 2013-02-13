{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Authenticate.BrowserId
    ( browserIdJs
    , checkAssertion
    ) where

import Data.Text (Text)
import Network.HTTP.Conduit (parseUrl, responseBody, httpLbs, Manager, method, urlEncodedBody)
import Data.Aeson (json, Value (Object, String))
import Data.Attoparsec.Lazy (parse, maybeResult)
import qualified Data.HashMap.Lazy as Map
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)

-- | Location of the Javascript file hosted by browserid.org
browserIdJs :: Text
browserIdJs = "https://login.persona.org/include.js"

checkAssertion :: (MonadResource m, MonadBaseControl IO m)
               => Text -- ^ audience
               -> Text -- ^ assertion
               -> Manager
               -> m (Maybe Text)
checkAssertion audience assertion manager = do
    req' <- liftIO $ parseUrl "https://verifier.login.persona.org/verify"
    let req = urlEncodedBody
                [ ("audience", encodeUtf8 audience)
                , ("assertion", encodeUtf8 assertion)
                ] req' { method = "POST" }
    res <- httpLbs req manager
    let lbs = responseBody res
    return $ maybeResult (parse json lbs) >>= getEmail
  where
    getEmail (Object o) =
        case (Map.lookup "status" o, Map.lookup "email" o) of
            (Just (String "okay"), Just (String e)) -> Just e
            _ -> Nothing
    getEmail _ = Nothing
