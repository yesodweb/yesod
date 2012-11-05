-- FIXME: Depend on the not-yet-released project-template library.
{-# LANGUAGE OverloadedStrings #-}
module MultiFile where

import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Trans.Resource (runExceptionT)
import qualified Data.ByteString              as S
import qualified Data.ByteString.Base64       as B64
import           Data.Conduit                 (Conduit, MonadResource, Sink,
                                               await, awaitForever, leftover,
                                               yield, ($$), (=$))
import           Data.Conduit.Binary          (sinkFile)
import           Data.Conduit.List            (sinkNull)
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Text            as CT
import           Data.Functor.Identity        (runIdentity)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (encodeUtf8)
import           Filesystem                   (createTree)
import           Filesystem.Path.CurrentOS    (FilePath, directory, encode,
                                               encodeString, fromText, (</>))
import           Prelude                      hiding (FilePath)

unpackMultiFile
    :: MonadResource m
    => FilePath -- ^ output folder
    -> (Text -> Text) -- ^ fix each input line, good for variables
    -> Sink S.ByteString m ()
unpackMultiFile root fixLine =
    CT.decode CT.utf8 =$ CT.lines =$ CL.map fixLine =$ start
  where
    start =
        await >>= maybe (return ()) go
      where
        go t =
            case getFileName t of
                Nothing -> error $ "Invalid input: " ++ show t
                Just (fp', isBinary) -> do
                    let fp = root </> fromText fp'
                    liftIO $ createTree $ directory fp
                    let src
                            | isBinary  = binaryLoop
                            | otherwise = textLoop
                    src =$ sinkFile (encodeString fp)
                    start

    binaryLoop = do
        await >>= maybe (error "binaryLoop needs 1 line") go
      where
        go = yield . B64.decodeLenient . encodeUtf8
    textLoop =
        await >>= maybe (return ()) go
      where
        go t =
            case getFileName t of
                Just{} -> leftover t
                Nothing -> do
                    yield $ encodeUtf8 t
                    yield "\n"
                    textLoop

    getFileName t =
        case T.words t of
            ["{-#", "START_FILE", fn, "#-}"] -> Just (fn, False)
            ["{-#", "START_FILE", "BASE64", fn, "#-}"] -> Just (fn, True)
            _ -> Nothing

createMultiFile
    :: MonadIO m
    => FilePath -- ^ folder containing the files
    -> Conduit FilePath m S.ByteString -- ^ FilePath is relative to containing folder
createMultiFile root = do
    awaitForever handleFile
  where
    handleFile fp' = do
        bs <- liftIO $ S.readFile $ encodeString fp
        case runIdentity $ runExceptionT $ yield bs $$ CT.decode CT.utf8 =$ sinkNull of
            Left{} -> do
                yield "{-# START_FILE BASE64 "
                yield $ encode fp'
                yield " #-}\n"
                yield $ B64.encode bs
                yield "\n"
            Right{} -> do
                yield "{-# START_FILE "
                yield $ encode fp'
                yield " #-}\n"
                yield bs
                unless ("\n" `S.isSuffixOf` bs) $ yield "\n"
      where
        fp = root </> fp'
