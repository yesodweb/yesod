module Yesod.Core.Types.HandlerContents
  (
    HandlerContents (..)
  ) where

import Control.Exception (Exception)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import Yesod.Core.Types.ErrorResponse
import Yesod.Core.Types.TypedContent (ContentType, TypedContent (..), typedContentToSnippet)

data HandlerContents =
      HCContent !H.Status !TypedContent
    | HCError !ErrorResponse
    | HCSendFile !ContentType !FilePath !(Maybe W.FilePart)
    | HCRedirect !H.Status !T.Text
    | HCCreated !T.Text
    | HCWai !W.Response
    | HCWaiApp !W.Application

instance Show HandlerContents where
    show (HCContent status tc@(TypedContent t _))
      = mconcat [ "HCContent "
                , show (status, t)
                , " ("
                , fromMaybe "" $ TL.unpack <$> typedContentToSnippet tc 1000
                , ")"
                ]
    show (HCError e) = "HCError " ++ show e
    show (HCSendFile ct fp mfp) = "HCSendFile " ++ show (ct, fp, mfp)
    show (HCRedirect s t) = "HCRedirect " ++ show (s, t)
    show (HCCreated t) = "HCCreated " ++ show t
    show (HCWai _) = "HCWai"
    show (HCWaiApp _) = "HCWaiApp"

instance Exception HandlerContents
