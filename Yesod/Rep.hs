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
    ( -- * Specific types of representations
      Plain (..)
    , plain
    , Template (..)
    , TemplateFile (..)
    , Static (..)
    , StaticFile (..)
#if TEST
    , testSuite
#endif
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import qualified Data.ByteString.Lazy as BL
import Web.Mime

#if TEST
import Data.Object.Html hiding (testSuite)
import Yesod.Response hiding (testSuite)
#else
import Data.Object.Html
import Yesod.Response
#endif

import Data.Object.Json
import Text.StringTemplate

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
#endif

newtype Plain = Plain { unPlain :: Text }
    deriving (Eq, Show)
instance HasReps Plain where
    chooseRep = defChooseRep [(TypePlain, return . cs . unPlain)]

plain :: ConvertSuccess x Text => x -> Plain
plain = Plain . cs

data Template = Template (StringTemplate Text)
                         String
                         HtmlObject
                         (IO [(String, HtmlObject)])
instance HasReps Template where
    chooseRep = defChooseRep [ (TypeHtml,
              \(Template t name ho attrsIO) -> do
                attrs <- attrsIO
                return
                    $ cs
                    $ render
                    $ setAttribute name ho
                    $ setManyAttrib attrs t)
           , (TypeJson, \(Template _ _ ho _) ->
                            return $ cs $ unJsonDoc $ cs ho)
           ]

-- FIXME
data TemplateFile = TemplateFile FilePath HtmlObject
instance HasReps TemplateFile where
    chooseRep = defChooseRep [ (TypeHtml,
              \(TemplateFile fp h) -> do
                    contents <- readFile fp
                    let t = newSTMP contents
                    return $ cs $ toString $ setAttribute "o" h t
             )
           , (TypeJson, \(TemplateFile _ ho) ->
                            return $ cs $ unJsonDoc $ cs ho)
           ]

data Static = Static ContentType ByteString
instance HasReps Static where
    chooseRep (Static ct bs) _ = return (ct, Content $ const $ return bs)

data StaticFile = StaticFile ContentType FilePath
instance HasReps StaticFile where
    chooseRep (StaticFile ct fp) _ = do
        bs <- BL.readFile fp
        return (ct, Content $ const $ return bs)

-- Useful instances of HasReps
instance HasReps HtmlObject where
    chooseRep = defChooseRep
        [ (TypeHtml, return . cs . unHtmlDoc . cs)
        , (TypeJson, return . cs . unJsonDoc . cs)
        ]

#if TEST
caseChooseRepHO :: Assertion
caseChooseRepHO = do
    {- FIXME
    let content = "IGNOREME"
        a = toHtmlObject content
        htmlbs = cs . unHtmlDoc . cs $ toHtmlObject content
        jsonbs = cs $ "\"" ++ content ++ "\""
    chooseRep a [TypeHtml] >>= (@?= (TypeHtml, htmlbs))
    chooseRep a [TypeJson] >>= (@?= (TypeJson, jsonbs))
    chooseRep a [TypeHtml, TypeJson] >>= (@?= (TypeHtml, htmlbs))
    chooseRep a [TypeOther "foo", TypeJson] >>= (@?= (TypeJson, jsonbs))
    -}
    return ()

caseChooseRepRaw :: Assertion
caseChooseRepRaw = do
    {- FIXME
    let content = Content $ cs "FOO"
        foo = TypeOther "foo"
        bar = TypeOther "bar"
        hasreps = [(TypeHtml, content), (foo, content)]
    chooseRep hasreps [TypeHtml] >>= (@?= (TypeHtml, content))
    chooseRep hasreps [foo, bar] >>= (@?= (foo, content))
    chooseRep hasreps [bar, foo] >>= (@?= (foo, content))
    chooseRep hasreps [bar]      >>= (@?= (TypeHtml, content))
    -}
    return ()

caseChooseRepTemplate :: Assertion
caseChooseRepTemplate = do
    {- FIXME
    let temp = newSTMP "foo:$o.foo$, bar:$o.bar$"
        ho = toHtmlObject [ ("foo", toHtmlObject "<fooval>")
                          , ("bar", Sequence $ map cs ["bar1", "bar2"])
                          ]
        hasreps = Template temp "o" ho $ return []
        res1 = cs "foo:&lt;fooval&gt;, bar:bar1bar2"
        res2 = cs $ "{\"bar\":[\"bar1\",\"bar2\"]," ++
                    "\"foo\":\"&lt;fooval&gt;\"}"
    chooseRep hasreps [TypeHtml]           >>= (@?= (TypeHtml, res1))
    chooseRep hasreps [TypeJson]           >>= (@?= (TypeJson, res2))
    chooseRep hasreps [TypeHtml, TypeJson] >>= (@?= (TypeHtml, res1))
    chooseRep hasreps [TypeJson, TypeHtml] >>= (@?= (TypeJson, res2))
    -}
    return ()

caseChooseRepTemplateFile :: Assertion
caseChooseRepTemplateFile = do
    {- FIXME
    let temp = "Test/rep.st"
        ho = toHtmlObject [ ("foo", toHtmlObject "<fooval>")
                          , ("bar", Sequence $ map cs ["bar1", "bar2"])
                          ]
        hasreps = TemplateFile temp ho
        res1 = cs "foo:&lt;fooval&gt;, bar:bar1bar2"
        res2 = cs $ "{\"bar\":[\"bar1\",\"bar2\"]," ++
                    "\"foo\":\"&lt;fooval&gt;\"}"
    chooseRep hasreps [TypeHtml]           >>= (@?= (TypeHtml, res1))
    chooseRep hasreps [TypeJson]           >>= (@?= (TypeJson, res2))
    chooseRep hasreps [TypeHtml, TypeJson] >>= (@?= (TypeHtml, res1))
    chooseRep hasreps [TypeJson, TypeHtml] >>= (@?= (TypeJson, res2))
    -}
    return ()

testSuite :: Test
testSuite = testGroup "Yesod.Rep"
    [ testCase "caseChooseRep HtmlObject" caseChooseRepHO
    , testCase "caseChooseRep raw" caseChooseRepRaw
    , testCase "caseChooseRep Template" caseChooseRepTemplate
    , testCase "caseChooseRep TemplateFile" caseChooseRepTemplateFile
    ]
#endif
