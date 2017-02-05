{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
module Yesod.EmbeddedStatic.Internal (
      EmbeddedStatic(..)
    , Route(..)
    , ComputedEntry(..)
    , devEmbed
    , prodEmbed
    , develApp
    , AddStaticContent
    , staticContentHelper
    , widgetSettings
) where

import Control.Applicative as A ((<$>))
import Data.IORef
import Language.Haskell.TH
import Network.HTTP.Types (Status(..), status404, status200, status304)
import Network.Mime (MimeType)
import Network.Wai
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import WaiAppStatic.Types
import Yesod.Core
          ( HandlerT
          , ParseRoute(..)
          , RenderRoute(..)
          , getYesod
          , liftIO
          )
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as M
import qualified WaiAppStatic.Storage.Embedded as Static

import Yesod.Static (base64md5)
import Yesod.EmbeddedStatic.Types

#if !MIN_VERSION_base(4,6,0)
-- copied from base
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref
            (\x -> let (a, b) = f x
                    in (a, a `seq` b))
    b `seq` return b
#endif

-- | The subsite for the embedded static file server.
data EmbeddedStatic = EmbeddedStatic {
    stApp :: !Application
  , widgetFiles :: !(IORef (M.HashMap T.Text File))
}

instance RenderRoute EmbeddedStatic where
    data Route EmbeddedStatic = EmbeddedResourceR [T.Text] [(T.Text,T.Text)]
                              | EmbeddedWidgetR T.Text
            deriving (Eq, Show, Read)
    renderRoute (EmbeddedResourceR x y) = ("res":x, y)
    renderRoute (EmbeddedWidgetR h) = (["widget",h], [])
instance ParseRoute EmbeddedStatic where
    parseRoute (("res":x), y) = Just $ EmbeddedResourceR x y
    parseRoute (["widget",h], _) = Just $ EmbeddedWidgetR h
    parseRoute _ = Nothing

-- | At compile time, one of these is created for every 'Entry' created by
-- the generators.  The cLink is a template haskell expression of type @Route EmbeddedStatic@.
data ComputedEntry = ComputedEntry {
      cHaskellName :: Maybe Name               -- ^ Optional haskell name to create a variable for the route
    , cStEntry     :: Static.EmbeddableEntry   -- ^ The entry to be embedded into the executable
    , cLink        :: ExpQ                     -- ^ The route for this entry
}

mkStr :: String -> ExpQ
mkStr = litE . stringL

-- | Create a 'ComputedEntry' for development mode, reloading the content on every request.
devEmbed :: Entry -> IO ComputedEntry
devEmbed e = return computed
    where
        st = Static.EmbeddableEntry {
                   Static.eLocation = "res/" `T.append` T.pack (ebLocation e)
                 , Static.eMimeType = ebMimeType e
                 , Static.eContent  = Right [| $(ebDevelReload e) >>= \c ->
                                               return (T.pack (base64md5 c), c) |]
                 }
        link = [| EmbeddedResourceR (T.splitOn (T.pack "/") $ T.pack $(mkStr $ ebLocation e)) [] |]
        computed = ComputedEntry (ebHaskellName e) st link

-- | Create a 'ComputedEntry' for production mode, hashing and embedding the content into the executable.
prodEmbed :: Entry -> IO ComputedEntry
prodEmbed e = do
    ct <- ebProductionContent e
    let hash = base64md5 ct
        link = [| EmbeddedResourceR (T.splitOn (T.pack "/") $ T.pack $(mkStr $ ebLocation e))
                                    [(T.pack "etag", T.pack $(mkStr hash))] |]
        st = Static.EmbeddableEntry {
                   Static.eLocation = "res/" `T.append` T.pack (ebLocation e)
                 , Static.eMimeType = ebMimeType e
                 , Static.eContent  = Left (T.pack hash, ct)
                 }
    return $ ComputedEntry (ebHaskellName e) st link

toApp :: (Request -> IO Response) -> Application
toApp f req g = f req >>= g

tryExtraDevelFiles :: [[T.Text] -> IO (Maybe (MimeType, BL.ByteString))] -> Application
tryExtraDevelFiles = toApp . tryExtraDevelFiles'

tryExtraDevelFiles' :: [[T.Text] -> IO (Maybe (MimeType, BL.ByteString))] -> Request -> IO Response
tryExtraDevelFiles' [] _ = return $ responseLBS status404 [] ""
tryExtraDevelFiles' (f:fs) r = do
    mct <- liftIO $ f $ drop 1 $ pathInfo r -- drop the initial "res"
    case mct of
        Nothing -> tryExtraDevelFiles' fs r
        Just (mime, ct) -> do
            let hash = T.encodeUtf8 $ T.pack $ base64md5 ct
            let headers = [ ("Content-Type", mime)
                          , ("ETag", hash)
                          ]
            case lookup "If-None-Match" (requestHeaders r) of
                Just h | hash == h -> return $ responseLBS status304 headers ""
                _ -> return $ responseLBS status200 headers ct

-- | Helper to create the development application at runtime
develApp :: StaticSettings -> [[T.Text] -> IO (Maybe (MimeType, BL.ByteString))] -> Application
develApp settings extra req sendResponse = do
    staticApp settings {ssMaxAge = NoMaxAge} req $ \resp ->
        if statusCode (responseStatus resp) == 404
            then tryExtraDevelFiles extra req sendResponse
            else sendResponse resp

-- | The type of 'addStaticContent'
type AddStaticContent site = T.Text -> T.Text -> BL.ByteString
                          -> HandlerT site IO (Maybe (Either T.Text (Route site, [(T.Text, T.Text)])))

-- | Helper for embedStaticContent and embedLicensedStaticContent.
staticContentHelper :: (site -> EmbeddedStatic)
                    -> (Route EmbeddedStatic -> Route site)
                    -> (BL.ByteString -> Either a BL.ByteString)
                    -> AddStaticContent site
staticContentHelper getStatic staticR minify ext _ ct = do
    wIORef <- widgetFiles . getStatic A.<$> getYesod
    let hash = T.pack $ base64md5 ct
        hash' = Just $ T.encodeUtf8 hash
        filename = T.concat [hash, ".", ext]
        content = case ext of
                    "js" -> either (const ct) id $ minify ct
                    _    -> ct
        file = File
                { fileGetSize = fromIntegral $ BL.length content
                , fileToResponse = \s h -> responseLBS s h content
                , fileName = unsafeToPiece filename
                , fileGetHash = return hash'
                , fileGetModified = Nothing
                }
    liftIO $ atomicModifyIORef' wIORef $ \m ->
        (M.insertWith (\old _ -> old) filename file m, ())

    return $ Just $ Right (staticR $ EmbeddedWidgetR filename, [])

-- | Create a wai-app-static settings based on the IORef inside the EmbeddedStaic site.
widgetSettings :: EmbeddedStatic -> StaticSettings
widgetSettings es = (defaultWebAppSettings "") { ssLookupFile = lookupFile }
    where
        lookupFile [_,p] = do -- The first part of the path is "widget"
            m <- readIORef $ widgetFiles es
            return $ maybe LRNotFound LRFile $ M.lookup (fromPiece p) m
        lookupFile _ = return LRNotFound
