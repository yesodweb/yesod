{-# LANGUAGE TupleSections, OverloadedStrings #-}
module HsFile (mkHsFile) where
import Text.ProjectTemplate (createTemplate)
import Data.Conduit 
    ( ($$), (=$), runResourceT, ResourceT, ConduitM, awaitForever, yield )
import Data.Conduit.Filesystem (traverse, sourceFile)
import Prelude hiding (FilePath)
import Filesystem.Path ( FilePath )
import Filesystem.Path.CurrentOS ( encodeString )
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)

mkHsFile :: IO ()
mkHsFile = runResourceT $ traverse False "."
        $$ readIt
        =$ createTemplate 
        =$ awaitForever (liftIO . BS.putStr)

-- Reads a filepath from upstream and dumps a pair of (filepath, filecontents)
readIt :: ConduitM FilePath (FilePath, ResourceT IO BS.ByteString) (ResourceT IO) ()
readIt = awaitForever $ \i -> do bs <- liftIO $ BS.readFile (encodeString i) 
                                 yield (i, return bs)


