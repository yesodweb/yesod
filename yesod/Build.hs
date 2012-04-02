{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Build
    ( getDeps
    , touchDeps
    , touch
    , recompDeps
    ) where

-- FIXME there's a bug when getFileStatus applies to a file
-- temporary deleted (e.g., Vim saving a file)

import           Control.Applicative ((<|>), many)
import qualified Data.Attoparsec.Text.Lazy as A
import           Data.Char (isSpace)
import qualified Data.Text.Lazy.IO as TIO

import           Control.Exception (SomeException, try)
import           Control.Monad (when, filterM, forM, forM_, (>=>))

import           Data.Monoid (mappend)
import           Data.List (isSuffixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified System.Posix.Types
import           System.Directory
import           System.FilePath (replaceExtension, (</>))
import           System.PosixCompat.Files (getFileStatus, setFileTimes,
                                             accessTime, modificationTime)
import Data.Char (isUpper)


touch :: IO ()
touch = touchDeps id updateFileTime =<< fmap snd (getDeps [])

recompDeps :: [FilePath] -> IO ()
recompDeps = getDeps >=> touchDeps hiFile removeHi . snd

type Deps = Map.Map FilePath (Set.Set FilePath)

getDeps :: [FilePath] -> IO ([FilePath], Deps)
getDeps hsSourceDirs = do
    let defSrcDirs = case hsSourceDirs of
                        [] -> ["."]
                        ds -> ds
    hss <- fmap concat $ mapM findHaskellFiles defSrcDirs
    deps' <- mapM determineDeps hss
    return $ (hss, fixDeps $ zip hss deps')

touchDeps :: (FilePath -> FilePath) ->
             (FilePath -> FilePath -> IO ()) ->
             Deps -> IO ()
touchDeps f action deps = (mapM_ go . Map.toList) deps
  where
    go (x, ys) =
        forM_ (Set.toList ys) $ \y -> do
            n <- x `isNewerThan` f y
            when n $ do
              putStrLn ("Forcing recompile for " ++ y ++ " because of " ++ x)
              action x y

-- | remove the .hi files for a .hs file, thereby forcing a recompile
removeHi :: FilePath -> FilePath -> IO ()
removeHi _ hs = mapM_ removeFile' hiFiles
    where
      removeFile' file = try' (removeFile file) >> return ()
      hiFiles          = map (\e -> "dist/build" </> replaceExtension hs e)
                             ["hi", "p_hi"]

-- | change file mtime of .hs file to that of the dependency
updateFileTime :: FilePath -> FilePath -> IO ()
updateFileTime x hs = do
  (_     , modx) <- getFileStatus' x
  (access, _   ) <- getFileStatus' hs
  _ <- try' (setFileTimes hs access modx)
  return ()

hiFile :: FilePath -> FilePath
hiFile hs = "dist/build" </> replaceExtension hs "hi"

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
    go ('.':_)          = return []
    go filename = do
        d <- doesDirectoryExist full
        if not d
          then if isHaskellFile
                  then return [full]
                  else return []
          else if isHaskellDir
                 then findHaskellFiles full
                 else return []
      where
        -- this could fail on unicode
        isHaskellDir = isUpper (head filename)
        isHaskellFile = ".hs" `isSuffixOf` filename || ".lhs" `isSuffixOf` filename
        full = path </> filename

data TempType = Verbatim | Messages FilePath | StaticFiles FilePath
#if __GLASGOW_HASKELL__ < 704
              | Hamlet 
#endif
    deriving Show

determineDeps :: FilePath -> IO [FilePath]
determineDeps x = do
    y <- TIO.readFile x -- FIXME catch IO exceptions
    let z = A.parse (many $ (parser <|> (A.anyChar >> return Nothing))) y
    case z of
        A.Fail{} -> return []
        A.Done _ r -> mapM go r >>= filterM doesFileExist . concat
  where
#if __GLASGOW_HASKELL__ < 704
    go (Just (Hamlet, f)) = return [f, "templates/" ++ f ++ ".hamlet"]
#endif
    go (Just (Verbatim, f)) = return [f]
    go (Just (Messages f, _)) = return [f]
    go (Just (StaticFiles fp, _)) = getFolderContents fp
    go Nothing = return []

    parser = do
        ty <- (do _ <- A.string "\nstaticFiles \""
                  x' <- A.many1 $ A.satisfy (/= '"')
                  return $ StaticFiles x')
#if __GLASGOW_HASKELL__ < 704
           <|> (A.string "$(parseRoutesFile " >> return Verbatim)
           <|> (A.string "$(hamletFile " >> return Hamlet)
           <|> (A.string "$(ihamletFile " >> return Hamlet)
           <|> (A.string "$(whamletFile " >> return Hamlet)
           <|> (A.string "$(html " >> return Hamlet)
           <|> (A.string "$(widgetFile " >> return Hamlet)
           <|> (A.string "$(Settings.hamletFile " >> return Hamlet)
           <|> (A.string "$(Settings.widgetFile " >> return Hamlet)
           <|> (A.string "$(persistFile " >> return Verbatim)
           <|> (
                   A.string "$(persistFileWith " >>
                   A.many1 (A.satisfy (/= '"')) >>
                   return Verbatim)
           <|> (do
                    _ <- A.string "\nmkMessage \""
                    A.skipWhile (/= '"')
                    _ <- A.string "\" \""
                    x' <- A.many1 $ A.satisfy (/= '"')
                    _ <- A.string "\" \""
                    y <- A.many1 $ A.satisfy (/= '"')
                    _ <- A.string "\""
                    return $ Messages $ concat [x', "/", y, ".msg"])
#endif
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
            notHidden ('t':"mp") = False
            notHidden _ = True
        fmap concat $ forM (filter notHidden cs) $ \c -> do
            let f = fp ++ '/' : c
            isFile <- doesFileExist f
            if isFile then return [f] else getFolderContents f
