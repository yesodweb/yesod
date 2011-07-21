{-# LANGUAGE OverloadedStrings #-}
module Web.Authenticate.BrowserId
    ( browserIdJs
    , checkAssertion
    ) where

import Data.Text (Text)
import Network.HTTP.Enumerator (parseUrl, responseBody, httpLbs, queryString, withManager)
import Network.HTTP.Types (queryTextToQuery)
import Data.Aeson (json, Value (Object, String))
import Data.Attoparsec.Lazy (parse, maybeResult)
import qualified Data.Map as Map

-- | Location of the Javascript file hosted by browserid.org
browserIdJs :: Text
browserIdJs = "https://browserid.org/include.js"

checkAssertion :: Text -- ^ audience
               -> Text -- ^ assertion
               -> IO (Maybe Text)
checkAssertion audience assertion = do
    req' <- parseUrl "https://browserid.org/verify"
    let req = req'
                { queryString = queryTextToQuery
                    [ ("audience", Just audience)
                    , ("assertion", Just assertion)
                    ]
                }
    res <- withManager $ httpLbs req
    let lbs = responseBody res
    return $ maybeResult (parse json lbs) >>= getEmail
  where
    getEmail (Object o) =
        case (Map.lookup "status" o, Map.lookup "email" o) of
            (Just (String "okay"), Just (String e)) -> Just e
            _ -> Nothing
    getEmail _ = Nothing
