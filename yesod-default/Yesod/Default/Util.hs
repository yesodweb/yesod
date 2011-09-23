{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Various utilities used in the scaffolded site.
module Yesod.Default.Util
    ( addStaticContentExternal
    , globFile
    , widgetFileProduction
    , widgetFileDebug
    ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text, pack, unpack)
import Yesod.Core -- purposely using complete import so that Haddock will see addStaticContent
import Control.Monad (unless)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Language.Haskell.TH.Syntax
import Text.Lucius (luciusFile, luciusFileDebug)
import Text.Julius (juliusFile, juliusFileDebug)
import Text.Cassius (cassiusFile, cassiusFileDebug)
import Data.Monoid (mempty)

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

-- | expects a root folder for each type, e.g: hamlet/ lucius/ julius/
globFile :: String -> String -> FilePath
globFile kind x = kind ++ "/" ++ x ++ "." ++ kind

widgetFileProduction :: FilePath -> Q Exp
widgetFileProduction x = do
    let h = whenExists x "hamlet"  whamletFile
    let c = whenExists x "cassius" cassiusFile
    let j = whenExists x "julius"  juliusFile
    let l = whenExists x "lucius"  luciusFile
    [|$h >> addCassius $c >> addJulius $j >> addLucius $l|]

widgetFileDebug :: FilePath -> Q Exp
widgetFileDebug x = do
    let h = whenExists x "hamlet"  whamletFile
    let c = whenExists x "cassius" cassiusFileDebug
    let j = whenExists x "julius"  juliusFileDebug
    let l = whenExists x "lucius"  luciusFileDebug
    [|$h >> addCassius $c >> addJulius $j >> addLucius $l|]

whenExists :: String -> String -> (FilePath -> Q Exp) -> Q Exp
whenExists x glob f = do
    let fn = globFile glob x
    e <- qRunIO $ doesFileExist fn
    if e then f fn else [|mempty|]
