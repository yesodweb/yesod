{-# LANGUAGE OverloadedStrings #-}
module Build
    ( copySources
    , getDeps
    , copyDeps
    , touch
    , findHaskellFiles
    ) where

-- FIXME there's a bug when getFileStatus applies to a file
-- temporary deleted (e.g., Vim saving a file)

import System.FilePath (takeFileName, takeDirectory, (</>))
import System.Directory
import Data.List (isSuffixOf)
import qualified Data.Attoparsec.Text.Lazy as A
import qualified Data.Text.Lazy.IO as TIO
import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (when, filterM, forM, forM_)
import Data.Char (isSpace)
import Data.Monoid (mappend)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.Posix.Types
import System.PosixCompat.Files (setFileTimes, getFileStatus,
                                             accessTime, modificationTime)


touch :: IO ()
touch = mapM_ go . Map.toList =<< getDeps
    where
      go (x, ys) = do
        (_, mod1) <- getFileStatus' x
        forM_ (Set.toList ys) $ \y -> do
            (access, mod2) <- getFileStatus' y
            when (mod2 < mod1) $ do
              putStrLn ("Touching " ++ y ++ " because of " ++ x)
              setFileTimes y access mod1


-- | Copy all .hs files to the devel src dir
copySources :: IO ()
copySources = cleanDev >> copySources'

copySources' :: IO ()
copySources' = do
    hss <- findHaskellFiles "."
    forM_ hss $ \hs -> do
        n <- hs `isNewerThan` (develSrcDir </> hs)
        when n (copyToDev hs)

type Deps = Map.Map FilePath (Set.Set FilePath)

develSrcDir :: FilePath
develSrcDir = "dist/src-devel"

getDeps :: IO Deps
getDeps = do
    hss <- findHaskellFiles "."
    deps' <- mapM determineHamletDeps hss
    return $ fixDeps $ zip hss deps'

copyDeps :: Deps -> IO ()
copyDeps deps = (mapM_ go . Map.toList) deps >> copySources'
  where
    go (x, ys) =
        forM_ (Set.toList ys) $ \y -> do
                  n <- x `isNewerThan` (develSrcDir </> y)
                  when n $ do
                    putStrLn ("Copying " ++ y ++ " because of " ++ x)
                    copyToDev y

copyToDev :: FilePath -> IO ()
copyToDev file = do
  createDirectoryIfMissing True targetDir
  copyFile file (targetDir </> takeFileName file)
  where
   dir = takeDirectory file
   targetDir = develSrcDir </> dir

cleanDev :: IO ()
cleanDev = do
  exists <- doesDirectoryExist develSrcDir
  when exists (removeDirectoryRecursive develSrcDir)

try' :: IO x -> IO (Either SomeException x)
try' = try

isNewerThan :: FilePath -> FilePath -> IO Bool
isNewerThan f1 f2 = do
  (_, mod1) <- getFileStatus' f1
  (_, mod2) <- getFileStatus' f2
  return (mod1 > mod2)

getFileStatus' :: FilePath ->
                  IO (System.Posix.Types.EpochTime, System.Posix.Types.EpochTime)
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
    go ('.':_)     = return []
    go "cabal-dev" = return []
    go "dist"      = return []
    go x = do
        let y = path </> x
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
