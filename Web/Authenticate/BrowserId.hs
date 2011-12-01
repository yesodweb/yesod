{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Web.Authenticate.BrowserId
    ( browserIdJs
    , checkAssertion
    ) where

import Data.Text (Text)
import Network.HTTP.Enumerator (parseUrl, responseBody, httpLbs, withManager, method, urlEncodedBody)
import Data.Aeson (json, Value (Object, String))
import Data.Attoparsec.Lazy (parse, maybeResult)
#if MIN_VERSION_aeson(0, 4, 0)
import qualified Data.HashMap.Lazy as Map
#else
import qualified Data.Map as Map
#endif
import Data.Text.Encoding (encodeUtf8)

-- | Location of the Javascript file hosted by browserid.org
browserIdJs :: Text
browserIdJs = "https://browserid.org/include.js"

checkAssertion :: Text -- ^ audience
               -> Text -- ^ assertion
               -> IO (Maybe Text)
checkAssertion audience assertion = do
    req' <- parseUrl "https://browserid.org/verify"
    let req = urlEncodedBody
                [ ("audience", encodeUtf8 audience)
                , ("assertion", encodeUtf8 assertion)
                ] req' { method = "POST" }
    res <- withManager $ httpLbs req
    let lbs = responseBody res
    return $ maybeResult (parse json lbs) >>= getEmail
  where
    getEmail (Object o) =
        case (Map.lookup "status" o, Map.lookup "email" o) of
            (Just (String "okay"), Just (String e)) -> Just e
            _ -> Nothing
    getEmail _ = Nothing
