{-# LANGUAGE OverloadedStrings #-}
module Build
    ( touch
    , getDeps
    , touchDeps
    , findHaskellFiles
    ) where

-- FIXME there's a bug when getFileStatus applies to a file temporary deleted (e.g., Vim saving a file)

import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import Data.List (isSuffixOf)
import qualified Data.Attoparsec.Text.Lazy as A
import qualified Data.Text.Lazy.IO as TIO
import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.Monoid (mappend)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.PosixCompat.Files (accessTime, modificationTime, getFileStatus, setFileTimes)
import qualified System.Posix.Types
import Control.Monad (filterM, forM)
import Control.Exception (SomeException, try)

-- | Touch any files with altered dependencies but do not build
touch :: IO ()
touch = do
    hss <- findHaskellFiles "."
    deps' <- mapM determineHamletDeps hss
    let deps = fixDeps $ zip hss deps'
    touchDeps deps

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

getFileStatus' :: FilePath -> IO (System.Posix.Types.EpochTime, System.Posix.Types.EpochTime)
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

data TempType = Hamlet | Verbatim | Messages FilePath | StaticFiles FilePath
    deriving Show

determineHamletDeps :: FilePath -> IO [FilePath]
determineHamletDeps x = do
    y <- TIO.readFile x -- FIXME catch IO exceptions
    let z = A.parse (A.many $ (parser <|> (A.anyChar >> return Nothing))) y
    case z of
        A.Fail{} -> return []
        A.Done _ r -> mapM go r >>= filterM doesFileExist . concat
  where
    go (Just (Hamlet, f)) = return [f, "hamlet/" ++ f ++ ".hamlet"]
    go (Just (Verbatim, f)) = return [f]
    go (Just (Messages f, _)) = return [f]
    go (Just (StaticFiles fp, _)) = getFolderContents fp
    go Nothing = return []
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
                    _ <- A.string "\nmkMessage \""
                    A.skipWhile (/= '"')
                    _ <- A.string "\" \""
                    x' <- A.many1 $ A.satisfy (/= '"')
                    _ <- A.string "\" \""
                    y <- A.many1 $ A.satisfy (/= '"')
                    _ <- A.string "\""
                    return $ Messages $ concat [x', "/", y, ".msg"])
           <|> (do
                    _ <- A.string "\nstaticFiles \""
                    x' <- A.many1 $ A.satisfy (/= '"')
                    return $ StaticFiles x')
        case ty of
            Messages{} -> return $ Just (ty, "")
            StaticFiles{} -> return $ Just (ty, "")
            _ -> do
                A.skipWhile isSpace
                _ <- A.char '"'
                y <- A.many1 $ A.satisfy (/= '"')
                _ <- A.char '"'
                A.skipWhile isSpace
                _ <- A.char ')'
                return $ Just (ty, y)

getFolderContents :: FilePath -> IO [FilePath]
getFolderContents fp = do
    cs <- getDirectoryContents fp
    let notHidden ('.':_) = False
        notHidden "tmp" = False
        notHidden _ = True
    fmap concat $ forM (filter notHidden cs) $ \c -> do
        let f = fp ++ '/' : c
        isFile <- doesFileExist f
        if isFile then return [f] else getFolderContents f
