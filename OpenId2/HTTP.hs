
--------------------------------------------------------------------------------
-- |
-- Module      : Network.OpenID.HTTP
-- Copyright   : (c) Trevor Elliott, 2008
-- License     : BSD3
--
-- Maintainer  : Trevor Elliott <trevor@geekgateway.com>
-- Stability   :
-- Portability :
--

module OpenId2.HTTP (
    -- * Request Interface
    makeRequest

    -- * Request/Response Parsing and Formatting
  , parseDirectResponse
  , formatParams
  , formatDirectParams
  , escapeParam
  , addParams
  , parseParams
  ) where

-- friends
import OpenId2.Types
--import Network.OpenID.Utils

-- libraries
import Data.List
import Network.BSD
import Network.Socket
import Network.URI hiding (query)
import Network.HTTP.Enumerator


-- | Perform an http request.
--   If the Bool parameter is set to True, redirects from the server will be
--   followed.
makeRequest :: Bool -> Resolver IO
makeRequest follow = if follow then httpLbsRedirect else httpLbs

-- Parsing and Formatting ------------------------------------------------------

-- | Turn a response body into a list of parameters.
parseDirectResponse :: String -> Params
parseDirectResponse  = unfoldr step
  where
    step []  = Nothing
    step str = case split (== '\n') str of
      (ps,rest) -> Just (split (== ':') ps,rest)


-- | Format OpenID parameters as a query string
formatParams :: Params -> String
formatParams  = intercalate "&" . map f
  where f (x,y) = x ++ "=" ++ escapeParam y


-- | Format OpenID parameters as a direct response
formatDirectParams :: Params -> String
formatDirectParams  = concatMap f
  where f (x,y) = x ++ ":" ++ y ++ "\n"


-- | Escape for the query string of a URI
escapeParam :: String -> String
escapeParam  = escapeURIString isUnreserved


-- | Add Parameters to a URI
addParams :: Params -> URI -> URI
addParams ps uri = uri { uriQuery = query }
  where
    f (k,v) = (k,v)
    ps' = map f ps
    query = '?' : formatParams (parseParams (uriQuery uri) ++ ps')


-- | Parse OpenID parameters out of a url string
parseParams :: String -> Params
parseParams xs = case split (== '?') xs of
  (_,bs) -> unfoldr step bs
  where
    step [] = Nothing
    step bs = case split (== '&') bs of
      (as,rest) -> case split (== '=') as of
        (k,v) -> Just ((k, unEscapeString v),rest)

split :: (a -> Bool) -> [a] -> ([a],[a])
split p as = case break p as of
  (xs,_:ys) -> (xs,ys)
  pair      -> pair
