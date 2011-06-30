{-# LANGUAGE OverloadedStrings #-}
module Scaffold.Build
    ( build
    , touch
    , getDeps
    , touchDeps
    , findHaskellFiles
    ) where

-- FIXME there's a bug when getFileStatus applies to a file temporary deleted (e.g., Vim saving a file)

import qualified Distribution.Simple.Build as B
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
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
import System.PosixCompat.Files (accessTime, modificationTime, getFileStatus, setFileTimes, FileStatus)
import Data.Text (unpack)
import Control.Monad (filterM)
import Control.Exception (SomeException, try)

-- | Touch any files with altered dependencies but do not build
touch :: IO ()
touch = do
    hss <- findHaskellFiles "."
    deps' <- mapM determineHamletDeps hss
    let deps = fixDeps $ zip hss deps'
    touchDeps deps

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
        (_, mod1) <- getFileStatus' x
        flip mapM_ (Set.toList ys) $ \y -> do
            (access, mod2) <- getFileStatus' y
            if mod2 < mod1
                then do
                    putStrLn $ "Touching " ++ y ++ " because of " ++ x
                    _ <- try' $ setFileTimes y access mod1
                    return ()
                else return ()

try' :: IO x -> IO (Either SomeException x)
try' = try

getFileStatus' fp = do
    efs <- try' $ getFileStatus fp
    case efs of
        Left _ -> return (0, 0)
        Right fs -> return (accessTime fs, modificationTime fs)

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

data TempType = Hamlet | Verbatim | Messages FilePath
    deriving Show

determineHamletDeps :: FilePath -> IO [FilePath]
determineHamletDeps x = do
    y <- TIO.readFile x -- FIXME catch IO exceptions
    let z = A.parse (A.many $ (parser <|> (A.anyChar >> return Nothing))) y
    case z of
        A.Fail{} -> return []
        A.Done _ r -> filterM doesFileExist $ concatMap go r
  where
    go (Just (Hamlet, f)) = [f, "hamlet/" ++ f ++ ".hamlet"]
    go (Just (Verbatim, f)) = [f]
    go (Just (Messages f, _)) = [f]
    go Nothing = []
    parser = do
        ty <- (A.string "$(hamletFile " >> return Hamlet)
           <|> (A.string "$(ihamletFile " >> return Hamlet)
           <|> (A.string "$(whamletFile " >> return Hamlet)
           <|> (A.string "$(html " >> return Hamlet)
           <|> (A.string "$(widgetFile " >> return Hamlet)
           <|> (A.string "$(Settings.hamletFile " >> return Hamlet)
           <|> (A.string "$(Settings.widgetFile " >> return Hamlet)
           <|> (A.string "$(persistFile " >> return Verbatim)
           <|> (A.string "$(parseRoutesFile " >> return Verbatim)
           <|> (do
                    A.string "\nmkMessage \""
                    A.skipWhile (/= '"')
                    A.string "\" \""
                    x <- A.many1 $ A.satisfy (/= '"')
                    A.string "\" \""
                    y <- A.many1 $ A.satisfy (/= '"')
                    A.string "\""
                    return $ Messages $ concat [x, "/", y, ".msg"])
        case ty of
            Messages{} -> return $ Just (ty, "")
            _ -> do
                A.skipWhile isSpace
                _ <- A.char '"'
                y <- A.many1 $ A.satisfy (/= '"')
                _ <- A.char '"'
                A.skipWhile isSpace
                _ <- A.char ')'
                return $ Just (ty, y)
