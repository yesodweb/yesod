{-# LANGUAGE TupleSections, OverloadedStrings #-}
module HsFile (mkHsFile) where
import Text.ProjectTemplate (createTemplate)
import Data.Conduit
import Data.Conduit.Binary (sourceFile, conduitFile, sinkHandle) 
import qualified Data.Conduit.List as CL
import Data.List (partition)
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import Filesystem.Path 
    ( FilePath, append, (</>), splitDirectories, filename
    , addExtension)
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ( (<$>) )
import System.Directory 
    ( getDirectoryContents, doesDirectoryExist
    , getCurrentDirectory, getTemporaryDirectory
    , copyFile, removeFile)
import System.IO (hClose, openBinaryFile ,IOMode(WriteMode))
import Data.Void (Void)

-- meh
doesDirectoryExistGood = doesDirectoryExist.encodeString
getDirectoryContentsGood fp =
    map decodeString <$> getDirectoryContents (encodeString fp)

-- Lists all the contents of a dir, including the files in subdirs
getAllDirContents :: FilePath -> IO [FilePath]
getAllDirContents dirname = do
    exists <- doesDirectoryExistGood dirname
    if exists
        then do
            contents <- getDirectoryContentsGood dirname
            let c = filter (\f -> f /= ".." && f /= ".") contents
            otherDirs <- mapM (doesDirectoryExistGood.append dirname) c
            let zippedDirs = zip c otherDirs
                filesAndDirs = (\(l1, l2) -> (fst $ unzip l1, fst $ unzip l2)) $
                                    partition (not.snd) zippedDirs
            dirs <- mapM getAllDirContents 
                        (map (append dirname) (snd filesAndDirs))
            return $ map (append dirname) (fst filesAndDirs) ++ (concat dirs)
        else return []

-- The final file should be dumped to a tmp directory
-- The name of the file is the same as the current directory name
destinationPath :: IO FilePath
destinationPath = do
    curdir <- getCurrentDirectory
    tmpdir <- getTemporaryDirectory
    exists <- doesDirectoryExist tmpdir
    let file = addExtension (filename $ decodeString curdir) "hsfiles"
        dir = decodeString $ if exists then tmpdir else ".."
    return $ dir </> file

-- This is it
mkHsFile :: IO ()
mkHsFile = do
    hsfile <- destinationPath
    infiles <- getAllDirContents "."
    runResourceT $ CL.sourceList infiles 
        $$ readIt
        =$ createTemplate 
        =$ sinkFileAndMoveIt (encodeString hsfile)

-- Reads a filepath from upstream and dumps a pair of 
readIt :: ConduitM FilePath (FilePath, ResourceT IO BS.ByteString) (ResourceT IO) ()
readIt = awaitForever $ \i -> do bs <- liftIO $ BS.readFile (encodeString i)
                                 yield (i, return bs)

-- Move the file from the temporary directory after the sink is finished
sinkFileAndMoveIt :: MonadResource m => String -> ConduitM BS.ByteString Void m ()
sinkFileAndMoveIt fp = 
    bracketP (openBinaryFile fp WriteMode) leGrandFinale sinkHandle
    where curDirFp = encodeString $ decodeString "." </> filename (decodeString fp)
          leGrandFinale h = do hClose h
                               copyFile fp curDirFp
                               removeFile fp 

