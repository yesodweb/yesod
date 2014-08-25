{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Build
    ( getDeps
    , touchDeps
    , touch
    , recompDeps
    , isNewerThan
    , safeReadFile
    ) where

import           Control.Applicative ((<|>), many, (<$>))
import qualified Data.Attoparsec.Text as A
import           Data.Char (isSpace, isUpper)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S

import           Control.Exception (SomeException, try, IOException)
import           Control.Exception.Lifted (handle)
import           Control.Monad (when, filterM, forM, forM_, (>=>))
import           Control.Monad.Trans.State (StateT, get, put, execStateT)
import           Control.Monad.Trans.Writer (WriterT, tell, execWriterT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)

import           Data.Monoid (Monoid (mappend, mempty))
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified System.Posix.Types
import           System.Directory
import           System.FilePath (takeExtension, replaceExtension, (</>), takeDirectory,
                                    splitPath, joinPath)
import           System.PosixCompat.Files (getFileStatus, setFileTimes,
                                             accessTime, modificationTime)

import           Text.Shakespeare (Deref)
import           Text.Julius      (juliusUsedIdentifiers)
import           Text.Cassius     (cassiusUsedIdentifiers)
import           Text.Lucius      (luciusUsedIdentifiers)

safeReadFile :: MonadIO m => FilePath -> m (Either IOException ByteString)
safeReadFile = liftIO . try . S.readFile

touch :: IO ()
touch = do
    m <- handle (\(_ :: SomeException) -> return Map.empty) $ readFile touchCache >>= readIO
    x <- fmap snd (getDeps [])
    m' <- execStateT (execWriterT $ touchDeps id updateFileTime x) m
    createDirectoryIfMissing True $ takeDirectory touchCache
    writeFile touchCache $ show m'
  where
    touchCache = "dist/touchCache.txt"

-- | Returns True if any files were touched, otherwise False
recompDeps :: [FilePath] -> StateT (Map.Map FilePath (Set.Set Deref)) IO Bool
recompDeps =
    fmap toBool . execWriterT . (liftIO . getDeps >=> touchDeps hiFile removeHi . snd)
  where
    toBool NoFilesTouched = False
    toBool SomeFilesTouched = True

type Deps = Map.Map FilePath ([FilePath], ComparisonType)

getDeps :: [FilePath] -> IO ([FilePath], Deps)
getDeps hsSourceDirs = do
    let defSrcDirs = case hsSourceDirs of
                        [] -> ["."]
                        ds -> ds
    hss <- fmap concat $ mapM findHaskellFiles defSrcDirs
    deps' <- mapM determineDeps hss
    return $ (hss, fixDeps $ zip hss deps')

data AnyFilesTouched = NoFilesTouched | SomeFilesTouched
instance Monoid AnyFilesTouched where
    mempty = NoFilesTouched
    mappend NoFilesTouched NoFilesTouched = mempty
    mappend _ _ = SomeFilesTouched

touchDeps :: (FilePath -> FilePath) ->
             (FilePath -> FilePath -> IO ()) ->
             Deps -> WriterT AnyFilesTouched (StateT (Map.Map FilePath (Set.Set Deref)) IO) ()
touchDeps f action deps = (mapM_ go . Map.toList) deps
  where
    go (x, (ys, ct)) = do
        isChanged <- handle (\(_ :: SomeException) -> return True) $ lift $
            case ct of
                AlwaysOutdated -> return True
                CompareUsedIdentifiers getDerefs -> do
                    derefMap <- get
                    ebs <- safeReadFile x
                    let newDerefs =
                            case ebs of
                                Left _ -> Set.empty
                                Right bs -> Set.fromList $ getDerefs $ T.unpack $ decodeUtf8With lenientDecode bs
                    put $ Map.insert x newDerefs derefMap
                    case Map.lookup x derefMap of
                        Just oldDerefs | oldDerefs == newDerefs -> return False
                        _ -> return True
        when isChanged $ forM_ ys $ \y -> do
            n <- liftIO $ x `isNewerThan` f y
            when n $ do
                liftIO $ putStrLn ("Forcing recompile for " ++ y ++ " because of " ++ x)
                liftIO $ action x y
                tell SomeFilesTouched

-- | remove the .hi files for a .hs file, thereby forcing a recompile
removeHi :: FilePath -> FilePath -> IO ()
removeHi _ hs = mapM_ removeFile' hiFiles
    where
      removeFile' file = try' (removeFile file) >> return ()
      hiFiles          = map (\e -> "dist/build" </> removeSrc (replaceExtension hs e))
                             ["hi", "p_hi"]

-- | change file mtime of .hs file to that of the dependency
updateFileTime :: FilePath -> FilePath -> IO ()
updateFileTime x hs = do
  (_     , modx) <- getFileStatus' x
  (access, _   ) <- getFileStatus' hs
  _ <- try' (setFileTimes hs access modx)
  return ()

hiFile :: FilePath -> FilePath
hiFile hs = "dist/build" </> removeSrc (replaceExtension hs "hi")

removeSrc :: FilePath -> FilePath
removeSrc f = case splitPath f of
    ("src/" : xs) -> joinPath xs
    _ -> f

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

fixDeps :: [(FilePath, [(ComparisonType, FilePath)])] -> Deps
fixDeps =
    Map.unionsWith combine . map go
  where
    go :: (FilePath, [(ComparisonType, FilePath)]) -> Deps
    go (x, ys) = Map.fromList $ map (\(ct, y) -> (y, ([x], ct))) ys

    combine (ys1, ct) (ys2, _) = (ys1 `mappend` ys2, ct)

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
        isHaskellDir  = isUpper (head filename)
        isHaskellFile = takeExtension filename `elem` watch_files
        full = path </> filename
        watch_files = [".hs", ".lhs"]

data TempType = StaticFiles FilePath
              | Verbatim | Messages FilePath | Hamlet | Widget | Julius | Cassius | Lucius
    deriving Show

-- | How to tell if a file is outdated.
data ComparisonType = AlwaysOutdated
                    | CompareUsedIdentifiers (String -> [Deref])

determineDeps :: FilePath -> IO [(ComparisonType, FilePath)]
determineDeps x = do
    y <- safeReadFile x
    case y of
        Left _ -> return []
        Right bs -> do
            let z = A.parseOnly (many $ (parser <|> (A.anyChar >> return Nothing)))
                  $ decodeUtf8With lenientDecode bs
            case z of
                Left _ -> return []
                Right r -> mapM go r >>= filterM (doesFileExist . snd) . concat
  where
    go (Just (StaticFiles fp, _)) = map ((,) AlwaysOutdated) <$> getFolderContents fp
    go (Just (Hamlet, f)) = return [(AlwaysOutdated, f)]
    go (Just (Widget, f)) = return
        [ (AlwaysOutdated, "templates/" ++ f ++ ".hamlet")
        , (CompareUsedIdentifiers $ map fst . juliusUsedIdentifiers, "templates/" ++ f ++ ".julius")
        , (CompareUsedIdentifiers $ map fst . luciusUsedIdentifiers, "templates/" ++ f ++ ".lucius")
        , (CompareUsedIdentifiers $ map fst . cassiusUsedIdentifiers, "templates/" ++ f ++ ".cassius")
        ]
    go (Just (Julius, f)) = return [(CompareUsedIdentifiers $ map fst . juliusUsedIdentifiers, f)]
    go (Just (Cassius, f)) = return [(CompareUsedIdentifiers $ map fst . cassiusUsedIdentifiers, f)]
    go (Just (Lucius, f)) = return [(CompareUsedIdentifiers $ map fst . luciusUsedIdentifiers, f)]
    go (Just (Verbatim, f)) = return [(AlwaysOutdated, f)]
    go (Just (Messages f, _)) = map ((,) AlwaysOutdated) <$> getFolderContents f
    go Nothing = return []

    parser = do
        ty <- (do _ <- A.string "\nstaticFiles \""
                  x' <- A.many1 $ A.satisfy (/= '"')
                  return $ StaticFiles x')
           <|> (A.string "$(parseRoutesFile " >> return Verbatim)
           <|> (A.string "$(hamletFile " >> return Hamlet)
           <|> (A.string "$(ihamletFile " >> return Hamlet)
           <|> (A.string "$(whamletFile " >> return Hamlet)
           <|> (A.string "$(html " >> return Hamlet)
           <|> (A.string "$(widgetFile " >> return Widget)
           <|> (A.string "$(Settings.hamletFile " >> return Hamlet)
           <|> (A.string "$(Settings.widgetFile " >> return Widget)
           <|> (A.string "$(juliusFile " >> return Julius)
           <|> (A.string "$(cassiusFile " >> return Cassius)
           <|> (A.string "$(luciusFile " >> return Lucius)
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
                    _y <- A.many1 $ A.satisfy (/= '"')
                    _ <- A.string "\""
                    return $ Messages x')
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
            notHidden ('f':"ay") = False
            notHidden _ = True
        fmap concat $ forM (filter notHidden cs) $ \c -> do
            let f = fp ++ '/' : c
            isFile <- doesFileExist f
            if isFile then return [f] else getFolderContents f
