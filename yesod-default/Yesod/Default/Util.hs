{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
-- | Various utilities used in the scaffolded site.
module Yesod.Default.Util
    ( addStaticContentExternal
    , globFile
    , widgetFileNoReload
    , widgetFileReload
    , widgetFileJsCss
    ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text, pack, unpack)
import Yesod.Core -- purposely using complete import so that Haddock will see addStaticContent
import Control.Monad (when, unless)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Language.Haskell.TH.Syntax
import Text.Lucius (luciusFile, luciusFileReload)
import Text.Julius (juliusFile, juliusFileReload)
import Text.Cassius (cassiusFile, cassiusFileReload)
import Data.Maybe (catMaybes)

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
    -> GHandler sub master (Maybe (Either Text (Route master, [(Text, Text)])))
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

widgetFileNoReload :: FilePath -> Q Exp
widgetFileNoReload x = combine
    [ whenExists x "hamlet"  whamletFile
    , whenExists x "cassius" cassiusFile
    , whenExists x "julius"  juliusFile
    , whenExists x "lucius"  luciusFile
    ]

widgetFileReload :: FilePath -> Q Exp
widgetFileReload x = combine
    [ whenExists x "hamlet"  whamletFile
    , whenExists x "cassius" cassiusFileReload
    , whenExists x "julius"  juliusFileReload
    , whenExists x "lucius"  luciusFileReload
    ]

widgetFileJsCss :: (String, FilePath -> Q Exp) -- ^ Css file extenstion and loading function. example: ("cassius", cassiusFileReload)
                -> (String, FilePath -> Q Exp) -- ^ Css file extenstion and loading function. example: ("julius", juliusFileReload)
                -> FilePath -> Q Exp
widgetFileJsCss (jsExt, jsLoad) (csExt, csLoad) x = combine
    [ whenExists x "hamlet"  whamletFile
    , whenExists x csExt csLoad
    , whenExists x jsExt jsLoad
    ]

combine :: [Q (Maybe Exp)] -> Q Exp
combine qmexps = do
    mexps <- sequence qmexps
    case catMaybes mexps of
        [] -> [|return ()|]
        exps -> do
            tw <- [|toWidget|]
            let exps' = map (AppE tw) exps
            return $ DoE $ map NoBindS exps'

whenExists :: String -> String -> (FilePath -> Q Exp) -> Q (Maybe Exp)
whenExists = warnUnlessExists False

warnUnlessExists :: Bool -> String -> String -> (FilePath -> Q Exp) -> Q (Maybe Exp)
warnUnlessExists shouldWarn x glob f = do
    let fn = globFile glob x
    e <- qRunIO $ doesFileExist fn
    when (shouldWarn && not e) $ qRunIO $ putStrLn $ "widget file not found: " ++ fn
    if e then fmap Just $ f fn else return Nothing
