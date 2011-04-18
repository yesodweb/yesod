{-# LANGUAGE OverloadedStrings #-}
module Scaffold.Build
    ( build
    , getDeps
    , touchDeps
    , findHaskellFiles
    ) where

-- FIXME there's a bug when getFileStatus applies to a file temporary deleted (e.g., Vim saving a file)

import qualified Distribution.Simple.Build as B
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Data.List (isSuffixOf)
import Distribution.Simple.Setup (defaultBuildFlags)
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Simple.LocalBuildInfo
import qualified Data.Attoparsec.Text.Lazy as A
import qualified Data.Text.Lazy.IO as TIO
import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Data.Monoid (mappend)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.PosixCompat.Files (accessTime, modificationTime, getFileStatus, setFileTimes)

build :: IO ()
build = do
    {-
    cabal <- defaultPackageDesc normal
    gpd <- readPackageDescription normal cabal
    putStrLn $ showPackageDescription $ packageDescription gpd
    -}
    hss <- findHaskellFiles "."
    deps' <- mapM determineHamletDeps hss
    let deps = fixDeps $ zip hss deps'
    touchDeps deps

    lbi <- getPersistBuildConfig "dist"
    B.build
        (localPkgDescr lbi)
        lbi
        defaultBuildFlags
        []

type Deps = Map.Map FilePath (Set.Set FilePath)

getDeps :: IO Deps
getDeps = do
    hss <- findHaskellFiles "."
    deps' <- mapM determineHamletDeps hss
    return $ fixDeps $ zip hss deps'

touchDeps :: Deps -> IO ()
touchDeps =
    mapM_ go . Map.toList
  where
    go (x, ys) = do
        fs <- getFileStatus x
        flip mapM_ (Set.toList ys) $ \y -> do
            fs' <- getFileStatus y
            if modificationTime fs' < modificationTime fs
                then do
                    putStrLn $ "Touching " ++ y ++ " because of " ++ x
                    setFileTimes y (accessTime fs') (modificationTime fs)
                else return ()

fixDeps :: [(FilePath, [FilePath])] -> Deps
fixDeps =
    Map.unionsWith mappend . map go
  where
    go :: (FilePath, [FilePath]) -> Deps
    go (x, ys) = Map.fromList $ map (\y -> (y, Set.singleton x)) ys

findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles path = do
    contents <- getDirectoryContents path
    fmap concat $ mapM go contents
  where
    go ('.':_) = return []
    go "dist" = return []
    go x = do
        let y = path ++ '/' : x
        d <- doesDirectoryExist y
        if d
            then findHaskellFiles y
            else if ".hs" `isSuffixOf` x || ".lhs" `isSuffixOf` x
                    then return [y]
                    else return []

data TempType = Hamlet | Cassius | Lucius | Julius | Widget | Verbatim
    deriving Show

determineHamletDeps :: FilePath -> IO [FilePath]
determineHamletDeps x = do
    y <- TIO.readFile x
    let z = A.parse (A.many $ (parser <|> (A.anyChar >> return Nothing))) y
    case z of
        A.Fail{} -> return []
        A.Done _ r -> return $ mapMaybe go r
  where
    go (Just (Hamlet, f)) = Just $ "hamlet/" ++ f ++ ".hamlet"
    go (Just (Widget, f)) = Just $ "hamlet/" ++ f ++ ".hamlet"
    go (Just (Verbatim, f)) = Just f
    go _ = Nothing
    parser = do
        ty <- (A.string "$(hamletFile " >> return Hamlet)
           <|> (A.string "$(cassiusFile " >> return Cassius)
           <|> (A.string "$(luciusFile " >> return Lucius)
           <|> (A.string "$(juliusFile " >> return Julius)
           <|> (A.string "$(widgetFile " >> return Widget)
           <|> (A.string "$(Settings.hamletFile " >> return Hamlet)
           <|> (A.string "$(Settings.cassiusFile " >> return Cassius)
           <|> (A.string "$(Settings.luciusFile " >> return Lucius)
           <|> (A.string "$(Settings.juliusFile " >> return Julius)
           <|> (A.string "$(Settings.widgetFile " >> return Widget)
           <|> (A.string "$(persistFile " >> return Verbatim)
           <|> (A.string "$(parseRoutesFile " >> return Verbatim)
        A.skipWhile isSpace
        _ <- A.char '"'
        y <- A.many1 $ A.satisfy (/= '"')
        _ <- A.char '"'
        A.skipWhile isSpace
        _ <- A.char ')'
        return $ Just (ty, y)
