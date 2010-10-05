{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Network.OpenID.Discovery
-- Copyright   : (c) Trevor Elliott, 2008
-- License     : BSD3
--
-- Maintainer  : Trevor Elliott <trevor@geekgateway.com>
-- Stability   :
-- Portability :
--

module OpenId2.Discovery (
    -- * Discovery
    discover
  ) where

-- Friends
import OpenId2.Types
import OpenId2.XRDS

-- Libraries
import Data.Char
import Data.List
import Data.Maybe
import Network.HTTP.Enumerator
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.Char8 as S8
import Control.Arrow (first)
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Failure (Failure (failure))

-- | Attempt to resolve an OpenID endpoint, and user identifier.
discover :: (MonadIO m, Failure OpenIdException m)
         => Resolver IO
         -> Identifier
         -> m (Provider, Identifier)
discover resolve ident@(Identifier i) = do
    res1 <- liftIO $ discoverYADIS resolve ident Nothing
    case res1 of
        Just x -> return x
        Nothing -> do
            res2 <- liftIO $ discoverHTML resolve ident
            case res2 of
                Just x -> return x
                Nothing -> failure $ DiscoveryException i

-- YADIS-Based Discovery -------------------------------------------------------

-- | Attempt a YADIS based discovery, given a valid identifier.  The result is
--   an OpenID endpoint, and the actual identifier for the user.
discoverYADIS :: Resolver IO
              -> Identifier
              -> Maybe String
              -> IO (Maybe (Provider,Identifier))
discoverYADIS resolve ident mb_loc = do
    let uri = fromMaybe (getIdentifier ident) mb_loc
    req <- parseUrl uri
    res <- httpLbs req
    let mloc = lookup "x-xrds-location"
             $ map (first $ map toLower . S8.unpack)
             $ responseHeaders res
    case statusCode res of
        200 ->
          case mloc of
            Just loc -> discoverYADIS resolve ident (Just $ S8.unpack loc)
            Nothing  -> do
              let mdoc = parseXRDS $ BSLU.toString $ responseBody res
              case mdoc of
                  Just doc -> return $ parseYADIS ident doc
                  Nothing -> return Nothing
        _ -> return Nothing


-- | Parse out an OpenID endpoint, and actual identifier from a YADIS xml
-- document.
parseYADIS :: Identifier -> XRDS -> Maybe (Provider,Identifier)
parseYADIS ident = listToMaybe . mapMaybe isOpenId . concat
  where
  isOpenId svc = do
    let tys = serviceTypes svc
        localId = maybe ident Identifier $ listToMaybe $ serviceLocalIDs svc
        f (x,y) | x `elem` tys = Just y
                | otherwise    = Nothing
    lid <- listToMaybe $ mapMaybe f
      [ ("http://specs.openid.net/auth/2.0/server", ident)
      -- claimed identifiers
      , ("http://specs.openid.net/auth/2.0/signon", localId)
      , ("http://openid.net/signon/1.0"           , localId)
      , ("http://openid.net/signon/1.1"           , localId)
      ]
    uri <- parseProvider =<< listToMaybe (serviceURIs svc)
    return (uri,lid)


-- HTML-Based Discovery --------------------------------------------------------

-- | Attempt to discover an OpenID endpoint, from an HTML document.  The result
-- will be an endpoint on success, and the actual identifier of the user.
discoverHTML :: Resolver IO -> Identifier -> IO (Maybe (Provider,Identifier))
discoverHTML resolve ident'@(Identifier ident) =
    parseHTML ident' . BSLU.toString <$> simpleHttp ident

-- | Parse out an OpenID endpoint and an actual identifier from an HTML
-- document.
parseHTML :: Identifier -> String -> Maybe (Provider,Identifier)
parseHTML ident = resolve
                . filter isOpenId
                . linkTags
                . htmlTags
  where
    isOpenId (rel,_) = "openid" `isPrefixOf` rel
    resolve ls = do
      prov <- parseProvider =<< lookup "openid2.provider" ls
      let lid = maybe ident Identifier $ lookup "openid2.local_id" ls
      return (prov,lid)


-- | Filter out link tags from a list of html tags.
linkTags :: [String] -> [(String,String)]
linkTags  = mapMaybe f . filter p
  where
    p = ("link " `isPrefixOf`)
    f xs = do
      let ys = unfoldr splitAttr (drop 5 xs)
      x <- lookup "rel"  ys
      y <- lookup "href" ys
      return (x,y)


-- | Split a string into strings of html tags.
htmlTags :: String -> [String]
htmlTags [] = []
htmlTags xs = case break (== '<') xs of
  (as,_:bs) -> fmt as : htmlTags bs
  (as,[])   -> [as]
  where
    fmt as = case break (== '>') as of
      (bs,_) -> bs


-- | Split out values from a key="value" like string, in a way that
-- is suitable for use with unfoldr.
splitAttr :: String -> Maybe ((String,String),String)
splitAttr xs = case break (== '=') xs of
  (_,[])         -> Nothing
  (key,_:'"':ys) -> f key (== '"') ys
  (key,_:ys)     -> f key isSpace  ys
  where
  f key p cs = case break p cs of
      (_,[])         -> Nothing
      (value,_:rest) -> Just ((key,value), dropWhile isSpace rest)
