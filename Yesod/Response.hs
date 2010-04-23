{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
---------------------------------------------------------
--
-- Module        : Yesod.Response
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Generating responses.
--
---------------------------------------------------------
module Yesod.Response
    ( -- * Content
      Content (..)
    , toContent
      -- * Representations
    , ChooseRep
    , HasReps (..)
    , defChooseRep
      -- ** Specific content types
    , RepHtml (..)
    , RepJson (..)
    , RepHtmlJson (..)
    , RepPlain (..)
    , RepXml (..)
      -- * Special responses
    , RedirectType (..)
    , SpecialResponse (..)
      -- * Error responses
    , ErrorResponse (..)
      -- * Header
    , Header (..)
    ) where

import Data.Maybe (mapMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy (Text)
import qualified Data.Text as T
import Data.Convertible.Text

import qualified Network.Wai as W
import qualified Network.Wai.Enumerator as WE

import Yesod.Request
import Web.Mime

-- | There are two different methods available for providing content in the
-- response: via files and enumerators. The former allows server to use
-- optimizations (usually the sendfile system call) for serving static files.
-- The latter is a space-efficient approach to content.
--
-- It can be tedious to write enumerators; often times, you will be well served
-- to use 'toContent'.
data Content = ContentFile FilePath
             | ContentEnum (forall a.
                             (a -> B.ByteString -> IO (Either a a))
                          -> a
                          -> IO (Either a a))

instance ConvertSuccess B.ByteString Content where
    convertSuccess bs = ContentEnum $ \f a -> f a bs
instance ConvertSuccess L.ByteString Content where
    convertSuccess = swapEnum . WE.fromLBS
instance ConvertSuccess T.Text Content where
    convertSuccess t = cs (cs t :: B.ByteString)
instance ConvertSuccess Text Content where
    convertSuccess lt = cs (cs lt :: L.ByteString)
instance ConvertSuccess String Content where
    convertSuccess s = cs (cs s :: Text)
instance ConvertSuccess (IO Text) Content where
    convertSuccess = swapEnum . WE.fromLBS' . fmap cs

-- | A synonym for 'convertSuccess' to make the desired output type explicit.
toContent :: ConvertSuccess x Content => x -> Content
toContent = cs

-- | A function which gives targetted representations of content based on the
-- content-types the user accepts.
type ChooseRep =
    [ContentType] -- ^ list of content-types user accepts, ordered by preference
 -> IO (ContentType, Content)

swapEnum :: W.Enumerator -> Content
swapEnum (W.Enumerator e) = ContentEnum e

-- | Any type which can be converted to representations.
class HasReps a where
    chooseRep :: a -> ChooseRep

-- | A helper method for generating 'HasReps' instances.
--
-- This function should be given a list of pairs of content type and conversion
-- functions. If none of the content types match, the first pair is used.
defChooseRep :: [(ContentType, a -> IO Content)] -> a -> ChooseRep
defChooseRep reps a ts = do
  let (ct, c) =
        case mapMaybe helper ts of
            (x:_) -> x
            [] -> case reps of
                    [] -> error "Empty reps to defChooseRep"
                    (x:_) -> x
  c' <- c a
  return (ct, c')
        where
            helper ct = do
                c <- lookup ct reps
                return (ct, c)

instance HasReps ChooseRep where
    chooseRep = id

instance HasReps () where
    chooseRep = defChooseRep [(TypePlain, const $ return $ cs "")]

instance HasReps [(ContentType, Content)] where
    chooseRep a cts = return $
        case filter (\(ct, _) -> go ct `elem` map go cts) a of
            ((ct, c):_) -> (ct, c)
            _ -> case a of
                    (x:_) -> x
                    _ -> error "chooseRep [(ContentType, Content)] of empty"
      where
        go = simpleContentType . contentTypeToString

newtype RepHtml = RepHtml Content
instance HasReps RepHtml where
    chooseRep (RepHtml c) _ = return (TypeHtml, c)
newtype RepJson = RepJson Content
instance HasReps RepJson where
    chooseRep (RepJson c) _ = return (TypeJson, c)
data RepHtmlJson = RepHtmlJson Content Content
instance HasReps RepHtmlJson where
    chooseRep (RepHtmlJson html json) = chooseRep
        [ (TypeHtml, html)
        , (TypeJson, json)
        ]
newtype RepPlain = RepPlain Content
instance HasReps RepPlain where
    chooseRep (RepPlain c) _ = return (TypePlain, c)
newtype RepXml = RepXml Content
instance HasReps RepXml where
    chooseRep (RepXml c) _ = return (TypeXml, c)

-- | Different types of redirects.
data RedirectType = RedirectPermanent
                  | RedirectTemporary
                  | RedirectSeeOther
    deriving (Show, Eq)

-- | Special types of responses which should short-circuit normal response
-- processing.
data SpecialResponse =
      Redirect RedirectType String
    | SendFile ContentType FilePath
    deriving (Show, Eq)

-- | Responses to indicate some form of an error occurred. These are different
-- from 'SpecialResponse' in that they allow for custom error pages.
data ErrorResponse =
      NotFound
    | InternalError String
    | InvalidArgs [(ParamName, ParamError)]
    | PermissionDenied
    | BadMethod String
    deriving (Show, Eq)

----- header stuff
-- | Headers to be added to a 'Result'.
data Header =
    AddCookie Int String String
    | DeleteCookie String
    | Header String String
    deriving (Eq, Show)
