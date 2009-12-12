{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Representations of data. A representation is basically how you display
-- information in a certain mime-type. For example, tree-style data can easily
-- be displayed as both JSON and Yaml.
--
-- To save programmers\' fingers, the name of this module and all data types
-- and classes replaces the full word Representation with Rep.
--
-- This concept is core to a RESTful framework. For example, if a user goes to
-- /movies/star-wars/, they'll want a HTML page describing the Star Wars movie.
-- However, if you've written an Ajax front-end, they might want than
-- information in XML or JSON format. There could also be another web service
-- that requests this information in a binary format to save on bandwidth.
--
-- Since the vast majority of information that is dealt with in web
-- applications can be easily displayed using an 'Object', that is probably
-- your best bet on internal data format to use. If you need HTML escaping,
-- then specifically an 'HtmlObject' will be even better.
--
-- By the way, I said above that the vast majority of information can be
-- contained in an 'Object' easily. The key word here is \"easily\"; in fact,
-- all data can be contained in an 'Object'; however, some of it requires more
-- effort.
module Yesod.Rep
    (
      ContentType (..)
    , Content (..)
    , Rep
    , Reps
    , HasReps (..)
    , chooseRep
    -- FIXME TemplateFile or some such...
      -- * Specific types of representations
    , Plain (..)
    , plain
#if TEST
    , testSuite
#endif
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Control.Applicative

#if TEST
import Data.Object.Html hiding (testSuite)
#else
import Data.Object.Html
#endif

import Data.Object.Json
import Data.Convertible.Text

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
#endif

data ContentType =
    TypeHtml
    | TypePlain
    | TypeJson
    | TypeXml
    | TypeAtom
    | TypeJpeg
    | TypePng
    | TypeGif
    | TypeJavascript
    | TypeCss
    | TypeFlv
    | TypeOgv
    | TypeOctet
    | TypeOther String
instance Show ContentType where
    show TypeHtml = "text/html"
    show TypePlain = "text/plain"
    show TypeJson = "application/json"
    show TypeXml = "text/xml"
    show TypeAtom = "application/atom+xml"
    show TypeJpeg = "image/jpeg"
    show TypePng = "image/png"
    show TypeGif = "image/gif"
    show TypeJavascript = "text/javascript"
    show TypeCss = "text/css"
    show TypeFlv = "video/x-flv"
    show TypeOgv = "video/ogg"
    show TypeOctet = "application/octet-stream"
    show (TypeOther s) = s
instance Eq ContentType where
    x == y = show x == show y

newtype Content = Content { unContent :: ByteString }
    deriving (Eq, Show)

instance ConvertSuccess Text Content where
    convertSuccess = Content . cs
instance ConvertSuccess ByteString Content where
    convertSuccess = Content
instance ConvertSuccess String Content where
    convertSuccess = Content . cs

type Rep a = (ContentType, a -> Content)
type Reps a = [Rep a]

-- | Any type which can be converted to representations. There must be at least
-- one representation for each type.
class HasReps a where
    reps :: Reps a
instance HasReps [(ContentType, Content)] where
    reps = [(TypeOther "FIXME", const $ Content $ cs "FIXME")]

-- FIXME done badly, needs cleanup
chooseRep :: HasReps a
          => a
          -> [ContentType]
          -> (ContentType, Content)
chooseRep a ts =
    let choices = rs' ++ rs
        helper2 (ct, f) = (ct, f a)
     in if null rs
            then error "Invalid empty reps"
            else helper2 $ head choices
    where
        rs = reps
        rs' = filter (\r -> fst r `elem` ts) rs
        -- for type signature stuff
        _ignored = pure (undefined :: Content) `asTypeOf`
                   (snd (head rs) )

newtype Plain = Plain Text
    deriving (Eq, Show)

plain :: ConvertSuccess x Text => x -> Plain
plain = Plain . cs

-- Useful instances of HasReps
instance HasReps HtmlObject where
    reps =
        [ (TypeHtml, cs . unHtmlDoc . cs)
        , (TypeJson, cs . unJsonDoc . cs)
        ]

#if TEST
caseChooseRep :: Assertion
caseChooseRep = do
    let content = "IGNOREME"
        a = toHtmlObject content
        htmlbs = Content . cs . unHtmlDoc . cs $ toHtmlObject content
        jsonbs = Content . cs $ "\"" ++ content ++ "\""
    chooseRep a [TypeHtml] @?= (TypeHtml, htmlbs)
    chooseRep a [TypeJson] @?= (TypeJson, jsonbs)
    chooseRep a [TypeHtml, TypeJson] @?= (TypeHtml, htmlbs)
    chooseRep a [TypeOther "foo", TypeJson] @?= (TypeJson, jsonbs)

testSuite :: Test
testSuite = testGroup "Yesod.Rep"
    [ testCase "caseChooseRep" caseChooseRep
    ]
#endif
