{-# LANGUAGE OverloadedStrings #-}
module Yesod.Mail
    ( Boundary (..)
    , Mail (..)
    , Part (..)
    , Encoding (..)
    , renderMail
    , renderMail'
    , sendmail
    , Disposition (..)
    , renderSendMail
    ) where

import qualified Data.ByteString.Lazy as L
import Text.Blaze.Builder.Utf8
import Text.Blaze.Builder.Core
import Data.Monoid
import System.Random
import Control.Arrow
import System.Process
import System.IO
import System.Exit
import Codec.Binary.Base64 (encode)
import Control.Monad ((<=<))

newtype Boundary = Boundary { unBoundary :: String }
instance Random Boundary where
    randomR = const random
    random =
        first (Boundary . map toChar) . sequence' (replicate 10 (randomR (0, 61)))
      where
        sequence' [] g = ([], g)
        sequence' (f:fs) g =
            let (f', g') = f g
                (fs', g'') = sequence' fs g'
             in (f' : fs', g'')
        toChar i
            | i < 26 = toEnum $ i + fromEnum 'A'
            | i < 52 = toEnum $ i + fromEnum 'a' - 26
            | otherwise = toEnum $ i + fromEnum '0' - 52

data Mail = Mail
    { mailHeaders :: [(String, String)]
    , mailPlain :: String
    , mailParts :: [Part]
    }

data Encoding = None | Base64

data Part = Part
    { partType :: String -- ^ content type
    , partEncoding :: Encoding
    , partDisposition :: Disposition
    , partContent :: L.ByteString
    }

data Disposition = Inline | Attachment String

renderMail :: Boundary -> Mail -> L.ByteString
renderMail (Boundary b) (Mail headers plain parts) = toLazyByteString $ mconcat
    [ mconcat $ map showHeader headers
    , mconcat $ map showHeader
        [ ("MIME-Version", "1.0")
        , ("Content-Type", "multipart/mixed; boundary=\""
            ++ b ++ "\"")
        ]
    , fromByteString "\n"
    , fromString plain
    , mconcat $ map showPart parts
    , fromByteString "\n--"
    , fromString b
    , fromByteString "--"
    ]
  where
    showHeader (k, v) = mconcat
        [ fromString k
        , fromByteString ": "
        , fromString v
        , fromByteString "\n"
        ]
    showPart (Part contentType encoding disposition content) = mconcat
        [ fromByteString "\n--"
        , fromString b
        , fromByteString "\n"
        , showHeader ("Content-Type", contentType)
        , case encoding of
            None -> mempty
            Base64 -> showHeader ("Content-Transfer-Encoding", "base64")
        , case disposition of
            Inline -> mempty
            Attachment filename ->
                showHeader ("Content-Disposition", "attachment; filename=" ++ filename)
        , fromByteString "\n"
        , case encoding of
            None -> writeList writeByteString $ L.toChunks content
            Base64 -> fromString $ encode $ L.unpack content
        ]

renderMail' :: Mail -> IO L.ByteString
renderMail' m = do
    b <- randomIO
    return $ renderMail b m

sendmail :: L.ByteString -> IO ()
sendmail lbs = do
    (Just hin, _, _, phandle) <- createProcess $ (proc
        "/usr/sbin/sendmail" ["-t"]) { std_in = CreatePipe }
    L.hPut hin lbs
    hClose hin
    exitCode <- waitForProcess phandle
    case exitCode of
        ExitSuccess -> return ()
        _ -> error $ "sendmail exited with error code " ++ show exitCode

renderSendMail :: Mail -> IO ()
renderSendMail = sendmail <=< renderMail'
