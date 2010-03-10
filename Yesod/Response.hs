{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
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
    ( -- * Representations
      Content (..)
    , ChooseRep
    , HasReps (..)
    , defChooseRep
    , ioTextToContent
    , hoToJsonContent
      -- ** Convenience wrappers
    , staticRep
      -- ** Specific content types
    , RepHtml (..)
    , RepJson (..)
    , RepHtmlJson (..)
      -- * Response type
    , Response (..)
      -- * Special responses
    , RedirectType (..)
    , getRedirectStatus
    , SpecialResponse (..)
      -- * Error responses
    , ErrorResponse (..)
    , getStatus
      -- * Header
    , Header (..)
    , headerToPair
      -- * Converting to WAI values
    , responseToWaiResponse
#if TEST
      -- * Tests
    , testSuite
    , runContent
#endif
    ) where

import Data.Time.Clock
import Data.Maybe (mapMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy (Text)
import qualified Data.Text as T
import Data.Object.Json

import Web.Encodings (formatW3)
import qualified Network.Wai as W
import qualified Network.Wai.Enumerator as WE

#if TEST
import Yesod.Request hiding (testSuite)
import Data.Object.Html hiding (testSuite)
import Web.Mime hiding (testSuite)
#else
import Yesod.Request
import Data.Object.Html
import Web.Mime
#endif

#if TEST
import Test.Framework (testGroup, Test)
#endif

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
instance ConvertSuccess HtmlDoc Content where
    convertSuccess = cs . unHtmlDoc
instance ConvertSuccess XmlDoc Content where
    convertSuccess = cs . unXmlDoc

type ChooseRep = [ContentType] -> IO (ContentType, Content)

-- | It would be nice to simplify 'Content' to the point where this is
-- unnecesary.
ioTextToContent :: IO Text -> Content
ioTextToContent = swapEnum . WE.fromLBS' . fmap cs

swapEnum :: W.Enumerator -> Content
swapEnum (W.Enumerator e) = ContentEnum e

hoToJsonContent :: HtmlObject -> Content
hoToJsonContent = cs . unJsonDoc . cs

-- | Any type which can be converted to representations.
class HasReps a where
    chooseRep :: a -> ChooseRep

-- | A helper method for generating 'HasReps' instances.
defChooseRep :: [(ContentType, a -> IO Content)] -> a -> ChooseRep
defChooseRep reps a ts = do
  let (ct, c) =
        case mapMaybe helper ts of
            (x:_) -> x
            [] -> case reps of
                    [] -> error "Empty reps"
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
        case filter (\(ct, _) -> simpleContentType ct `elem`
                                  map simpleContentType cts) a of
            ((ct, c):_) -> (ct, c)
            _ -> case a of
                    (x:_) -> x
                    _ -> error "chooseRep [(ContentType, Content)] of empty"

instance HasReps (Html, HtmlObject) where
    chooseRep = defChooseRep
        [ (TypeHtml, return . cs . unHtmlDoc . cs)
        , (TypeJson, return . cs . unJsonDoc . cs)
        ]

-- | Data with a single representation.
staticRep :: ConvertSuccess x Content
          => ContentType
          -> x
          -> [(ContentType, Content)]
staticRep ct x = [(ct, cs x)]

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

data Response = Response W.Status [Header] ContentType Content

-- | Different types of redirects.
data RedirectType = RedirectPermanent
                  | RedirectTemporary
                  | RedirectSeeOther
    deriving (Show, Eq)

getRedirectStatus :: RedirectType -> W.Status
getRedirectStatus RedirectPermanent = W.Status301
getRedirectStatus RedirectTemporary = W.Status302
getRedirectStatus RedirectSeeOther = W.Status303

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
    deriving (Show, Eq)

getStatus :: ErrorResponse -> W.Status
getStatus NotFound = W.Status404
getStatus (InternalError _) = W.Status500
getStatus (InvalidArgs _) = W.Status400
getStatus PermissionDenied = W.Status403

----- header stuff
-- | Headers to be added to a 'Result'.
data Header =
    AddCookie Int String String
    | DeleteCookie String
    | Header String String
    deriving (Eq, Show)

-- | Convert Header to a key/value pair.
headerToPair :: Header -> IO (W.ResponseHeader, B.ByteString)
headerToPair (AddCookie minutes key value) = do
    now <- getCurrentTime
    let expires = addUTCTime (fromIntegral $ minutes * 60) now
    return (W.SetCookie, cs $ key ++ "=" ++ value ++"; path=/; expires="
                              ++ formatW3 expires)
headerToPair (DeleteCookie key) = return
    (W.SetCookie, cs $
     key ++ "=; path=/; expires=Thu, 01-Jan-1970 00:00:00 GMT")
headerToPair (Header key value) =
    return (W.responseHeaderFromBS $ cs key, cs value)

responseToWaiResponse :: Response -> IO W.Response
responseToWaiResponse (Response sc hs ct c) = do
    hs' <- mapM headerToPair hs
    let hs'' = (W.ContentType, cs ct) : hs'
    return $ W.Response sc hs'' $ case c of
                                    ContentFile fp -> Left fp
                                    ContentEnum e -> Right $ W.Enumerator e

#if TEST
runContent :: Content -> IO L.ByteString
runContent (ContentFile fp) = L.readFile fp
runContent (ContentEnum c) = WE.toLBS $ W.Enumerator c

----- Testing
testSuite :: Test
testSuite = testGroup "Yesod.Response"
    [
    ]
#endif
