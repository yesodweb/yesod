{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
-- | Various utilities used in the scaffolded site.
module Yesod.Default.Util
    ( addStaticContentExternal
    , globFile
    , widgetFileNoReload
    , widgetFileReload
    , TemplateLanguage (..)
    , defaultTemplateLanguages
    , WidgetFileSettings
    , wfsLanguages
    , wfsHamletSettings
    ) where

import qualified Data.ByteString.Lazy as L
import Data.Text (Text, pack, unpack)
import Yesod.Core -- purposely using complete import so that Haddock will see addStaticContent
import Control.Monad (when, unless)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Language.Haskell.TH.Syntax
import Text.Lucius (luciusFile, luciusFileReload)
import Text.Julius (juliusFile, juliusFileReload)
import Text.Cassius (cassiusFile, cassiusFileReload)
import Text.Hamlet (HamletSettings, defaultHamletSettings)
import Data.Maybe (catMaybes)
import Data.Default (Default (def))

-- | An implementation of 'addStaticContent' which stores the contents in an
-- external file. Files are created in the given static folder with names based
-- on a hash of their content. This allows expiration dates to be set far in
-- the future without worry of users receiving stale content.
addStaticContentExternal
    :: (L.ByteString -> Either a L.ByteString) -- ^ javascript minifier
    -> (L.ByteString -> String) -- ^ hash function to determine file name
    -> FilePath -- ^ location of static directory. files will be placed within a "tmp" subfolder
    -> ([Text] -> Route master) -- ^ route constructor, taking a list of pieces
    -> Text -- ^ filename extension
    -> Text -- ^ mime type
    -> L.ByteString -- ^ file contents
    -> HandlerT master IO (Maybe (Either Text (Route master, [(Text, Text)])))
addStaticContentExternal minify hash staticDir toRoute ext' _ content = do
    liftIO $ createDirectoryIfMissing True statictmp
    exists <- liftIO $ doesFileExist fn'
    unless exists $ liftIO $ L.writeFile fn' content'
    return $ Just $ Right (toRoute ["tmp", pack fn], [])
  where
    fn, statictmp, fn' :: FilePath
    -- by basing the hash off of the un-minified content, we avoid a costly
    -- minification if the file already exists
    fn = hash content ++ '.' : unpack ext'
    statictmp = staticDir ++ "/tmp/"
    fn' = statictmp ++ fn

    content' :: L.ByteString
    content'
        | ext' == "js" = either (const content) id $ minify content
        | otherwise = content

-- | expects a file extension for each type, e.g: hamlet lucius julius
globFile :: String -> String -> FilePath
globFile kind x = "templates/" ++ x ++ "." ++ kind

data TemplateLanguage = TemplateLanguage
    { tlRequiresToWidget :: Bool
    , tlExtension :: String
    , tlNoReload :: FilePath -> Q Exp
    , tlReload :: FilePath -> Q Exp
    }

defaultTemplateLanguages :: HamletSettings -> [TemplateLanguage]
defaultTemplateLanguages hset =
    [ TemplateLanguage False "hamlet"  whamletFile' whamletFile'
    , TemplateLanguage True  "cassius" cassiusFile  cassiusFileReload
    , TemplateLanguage True  "julius"  juliusFile   juliusFileReload
    , TemplateLanguage True  "lucius"  luciusFile   luciusFileReload
    ]
  where
    whamletFile' = whamletFileWithSettings hset

data WidgetFileSettings = WidgetFileSettings
    { wfsLanguages :: HamletSettings -> [TemplateLanguage]
    , wfsHamletSettings :: HamletSettings
    }

instance Default WidgetFileSettings where
    def = WidgetFileSettings defaultTemplateLanguages defaultHamletSettings

widgetFileNoReload :: WidgetFileSettings -> FilePath -> Q Exp
widgetFileNoReload wfs x = combine "widgetFileNoReload" x False $ wfsLanguages wfs $ wfsHamletSettings wfs

widgetFileReload :: WidgetFileSettings -> FilePath -> Q Exp
widgetFileReload wfs x = combine "widgetFileReload" x True $ wfsLanguages wfs $ wfsHamletSettings wfs

combine :: String -> String -> Bool -> [TemplateLanguage] -> Q Exp
combine func file isReload tls = do
    mexps <- qmexps
    case catMaybes mexps of
        [] -> error $ concat
            [ "Called "
            , func
            , " on "
            , show file
            , ", but no template were found."
            ]
        exps -> return $ DoE $ map NoBindS exps
  where
    qmexps :: Q [Maybe Exp]
    qmexps = mapM go tls

    go :: TemplateLanguage -> Q (Maybe Exp)
    go tl = whenExists file (tlRequiresToWidget tl) (tlExtension tl) ((if isReload then tlReload else tlNoReload) tl)

whenExists :: String
           -> Bool -- ^ requires toWidget wrap
           -> String -> (FilePath -> Q Exp) -> Q (Maybe Exp)
whenExists = warnUnlessExists False

warnUnlessExists :: Bool
                 -> String
                 -> Bool -- ^ requires toWidget wrap
                 -> String -> (FilePath -> Q Exp) -> Q (Maybe Exp)
warnUnlessExists shouldWarn x wrap glob f = do
    let fn = globFile glob x
    e <- qRunIO $ doesFileExist fn
    when (shouldWarn && not e) $ qRunIO $ putStrLn $ "widget file not found: " ++ fn
    if e
        then do
            ex <- f fn
            if wrap
                then do
                    tw <- [|toWidget|]
                    return $ Just $ tw `AppE` ex
                else return $ Just ex
        else return Nothing
