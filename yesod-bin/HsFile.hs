{-# LANGUAGE OverloadedStrings #-}
module HsFile (mkHsFile) where
import Text.ProjectTemplate (createTemplate)
import Data.Conduit 
    ( ($$), (=$), ConduitM, awaitForever, yield, Source )
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.Conduit.List as CL
import Prelude hiding (FilePath)
import Filesystem.Path ( FilePath )
import Filesystem.Path.CurrentOS ( encodeString )
import qualified Filesystem as F
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)

traverse :: FilePath -> Source (ResourceT IO) FilePath
traverse dir = do
    liftIO (F.listDirectory dir) >>= mapM_ go
  where
    go fp = do
        isFile' <- liftIO $ F.isFile fp
        if isFile'
            then yield fp
            else do
                isDir <- liftIO $ F.isDirectory fp
                if isDir
                    then traverse fp
                    else return ()

mkHsFile :: IO ()
mkHsFile = runResourceT $ traverse "."
        $$ readIt
        =$ createTemplate 
        =$ awaitForever (liftIO . BS.putStr)

-- Reads a filepath from upstream and dumps a pair of (filepath, filecontents)
readIt :: ConduitM FilePath (FilePath, ResourceT IO BS.ByteString) (ResourceT IO) ()
readIt = CL.map $ \i -> (i, liftIO $ BS.readFile $ encodeString i)

