{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    , Content
    , Rep
    , Reps
    , HasReps (..)
    , chooseRep
    -- FIXME TemplateFile or some such...
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
    | TypeJson
    | TypeOther String
    deriving Eq
instance Show ContentType where
    show TypeHtml = "text/html"
    show TypeJson = "application/json"
    show (TypeOther s) = s

newtype Content = Content ByteString
    deriving (Eq, Show)

instance ConvertSuccess Text Content where
    convertSuccess = Content . cs
instance ConvertSuccess ByteString Content where
    convertSuccess = Content

type Rep a = (ContentType, a -> Content)
type Reps a = [Rep a]

-- | Any type which can be converted to representations. There must be at least
-- one representation for each type.
class HasReps a where
    reps :: Reps a

chooseRep :: (Applicative f, HasReps a)
          => f a
          -> [ContentType]
          -> f (ContentType, Content)
chooseRep fa ts =
    let choices = rs' ++ rs
        helper2 (ct, f) =
            let fbs = f `fmap` fa
             in pure (\bs -> (ct, bs)) <*> fbs
     in if null rs
            then error "Invalid empty reps"
            else helper2 (head choices)
    where
        rs = reps
        rs' = filter (\r -> fst r `elem` ts) rs
        -- for type signature stuff
        _ignored = pure (undefined :: Content) `asTypeOf`
                   (snd (head rs) `fmap` fa)

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
        a = Just $ toHtmlObject content
        htmlbs = Content . cs . unHtmlDoc . cs $ toHtmlObject content
        jsonbs = Content . cs $ "\"" ++ content ++ "\""
    chooseRep a [TypeHtml] @?= Just (TypeHtml, htmlbs)
    chooseRep a [TypeJson] @?= Just (TypeJson, jsonbs)
    chooseRep a [TypeHtml, TypeJson] @?= Just (TypeHtml, htmlbs)
    chooseRep a [TypeOther "foo", TypeJson] @?= Just (TypeJson, jsonbs)

testSuite :: Test
testSuite = testGroup "Yesod.Rep"
    [ testCase "caseChooseRep" caseChooseRep
    ]
#endif
