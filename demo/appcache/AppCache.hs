{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module AppCache where

import           Control.Monad              (when)
import           Control.Monad.Trans.Writer
import           Data.Hashable              (hashWithSalt)
import           Data.List                  (intercalate)
import qualified Data.Set                   as Set
import           Data.Text                  (Text, pack)
import           Language.Haskell.TH.Syntax
import           Yesod.Core
import           Yesod.Routes.TH

newtype AppCache = AppCache { unAppCache :: Text }

appCache :: [ResourceTree String] -> Q Exp
appCache trees = do
    piecesSet <- execWriterT $ mapM_ (goTree id) trees
    let body = unlines $ map toPath $ Set.toList piecesSet
        hash = hashWithSalt 0 body
        total = concat
            [ "CACHE MANIFEST\n# Version: "
            , show hash
            , "\n\nCACHE:\n"
            , body
            ]
    [|return (AppCache (pack total))|]
  where
    toPath [] = "/"
    toPath x = concatMap ('/':) x

goTree :: Monad m
       => ([String] -> [String])
       -> ResourceTree String
       -> WriterT (Set.Set [String]) m ()
goTree front (ResourceLeaf res) = do
    pieces' <- goPieces (resourceName res) $ resourcePieces res
    when ("CACHE" `elem` resourceAttrs res) $
        tell $ Set.singleton $ front pieces'
goTree front (ResourceParent name pieces trees) = do
    pieces' <- goPieces name pieces
    mapM_ (goTree $ front . (pieces' ++)) trees

goPieces :: Monad m => String -> [(CheckOverlap, Piece String)] -> m [String]
goPieces name =
    mapM (goPiece . snd)
  where
    goPiece (Static s) = return s
    goPiece (Dynamic _) = fail $ concat
        [ "AppCache only applies to fully-static paths, but "
        , name
        , " has dynamic pieces."
        ]

instance ToContent AppCache where
    toContent = toContent . unAppCache
instance ToTypedContent AppCache where
    toTypedContent = TypedContent "text/cache-manifest" . toContent
