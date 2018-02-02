{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module HsFile (mkHsFile) where
import Text.ProjectTemplate (createTemplate)
import Conduit
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)

mkHsFile :: IO ()
mkHsFile = runConduitRes
         $ sourceDirectory "."
        .| readIt
        .| createTemplate
        .| mapM_C (liftIO . BS.putStr)
  where
    -- Reads a filepath from upstream and dumps a pair of (filepath, filecontents)
    readIt = mapC $ \i -> (fromString i, liftIO $ BS.readFile i)
