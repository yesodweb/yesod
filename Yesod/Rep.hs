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
    , RepChooser
    , ContentPair
    , HasReps (..)
      -- * Specific types of representations
    , Plain (..)
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
import qualified Data.ByteString.Lazy as BL
import Data.Text.Lazy (Text)
import Data.Maybe (mapMaybe)
import Data.Function (on)

#if TEST
import Data.Object.Html hiding (testSuite)
#else
import Data.Object.Html
#endif

import Data.Object.Json
import Data.Convertible.Text
import Text.StringTemplate

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
    deriving (Show)
instance ConvertSuccess ContentType String where
    convertSuccess TypeHtml = "text/html"
    convertSuccess TypePlain = "text/plain"
    convertSuccess TypeJson = "application/json"
    convertSuccess TypeXml = "text/xml"
    convertSuccess TypeAtom = "application/atom+xml"
    convertSuccess TypeJpeg = "image/jpeg"
    convertSuccess TypePng = "image/png"
    convertSuccess TypeGif = "image/gif"
    convertSuccess TypeJavascript = "text/javascript"
    convertSuccess TypeCss = "text/css"
    convertSuccess TypeFlv = "video/x-flv"
    convertSuccess TypeOgv = "video/ogg"
    convertSuccess TypeOctet = "application/octet-stream"
    convertSuccess (TypeOther s) = s
instance Eq ContentType where
    (==) = (==) `on` (cs :: ContentType -> String)

newtype Content = Content { unContent :: ByteString }
    deriving (Eq, Show)

instance ConvertSuccess Text Content where
    convertSuccess = Content . cs
instance ConvertSuccess ByteString Content where
    convertSuccess = Content
instance ConvertSuccess String Content where
    convertSuccess = Content . cs
instance ConvertSuccess Html Content where
    convertSuccess = Content . cs
instance ConvertSuccess XmlDoc Content where
    convertSuccess = cs . unXmlDoc

type ContentPair = (ContentType, Content)
type RepChooser = [ContentType] -> IO ContentPair

-- | Any type which can be converted to representations. There must be at least
-- one representation for each type.
class HasReps a where
    reps :: [(ContentType, a -> IO Content)]
    chooseRep :: a -> RepChooser
    chooseRep a ts = do
      let (ct, c) =
            case mapMaybe helper ts of
                (x:_) -> x
                [] -> case reps of
                        [] -> error "Empty reps"
                        (x:_) -> x
      c' <- c a
      return (ct, c')
            where
                --helper :: ContentType -> Maybe ContentPair
                helper ct = do
                    c <- lookup ct reps
                    return (ct, c)

instance HasReps RepChooser where
    reps = error "reps of RepChooser"
    chooseRep = id

instance HasReps () where
    reps = [(TypePlain, const $ return $ cs "")]

instance HasReps [(ContentType, Content)] where
    reps = error "reps of [(ContentType, Content)]"
    chooseRep a cts = return $
        case filter (\(ct, _) -> ct `elem` cts) a of
            ((ct, c):_) -> (ct, c)
            _ -> case a of
                    (x:_) -> x
                    _ -> error "chooseRep [(ContentType, Content)] of empty"

newtype Plain = Plain { unPlain :: Text }
    deriving (Eq, Show)
instance HasReps Plain where
    reps = [(TypePlain, return . cs . unPlain)]

plain :: ConvertSuccess x Text => x -> Plain
plain = Plain . cs

data Template = Template (StringTemplate Text)
                         String
                         HtmlObject
                         (IO [(String, HtmlObject)])
instance HasReps Template where
    reps = [ (TypeHtml,
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
    reps = [ (TypeHtml,
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
    reps = error "reps of Static"
    chooseRep (Static ct bs) _ = return (ct, Content bs)

data StaticFile = StaticFile ContentType FilePath
instance HasReps StaticFile where
    reps = error "reps of StaticFile"
    chooseRep (StaticFile ct fp) _ = do
        bs <- BL.readFile fp
        return (ct, Content bs)

-- Useful instances of HasReps
instance HasReps HtmlObject where
    reps =
        [ (TypeHtml, return . cs . unHtmlDoc . cs)
        , (TypeJson, return . cs . unJsonDoc . cs)
        ]

#if TEST
caseChooseRepHO :: Assertion
caseChooseRepHO = do
    let content = "IGNOREME"
        a = toHtmlObject content
        htmlbs = Content . cs . unHtmlDoc . cs $ toHtmlObject content
        jsonbs = Content . cs $ "\"" ++ content ++ "\""
    chooseRep a [TypeHtml] >>= (@?= (TypeHtml, htmlbs))
    chooseRep a [TypeJson] >>= (@?= (TypeJson, jsonbs))
    chooseRep a [TypeHtml, TypeJson] >>= (@?= (TypeHtml, htmlbs))
    chooseRep a [TypeOther "foo", TypeJson] >>= (@?= (TypeJson, jsonbs))

caseChooseRepRaw :: Assertion
caseChooseRepRaw = do
    let content = Content $ cs "FOO"
        foo = TypeOther "foo"
        bar = TypeOther "bar"
        hasreps = [(TypeHtml, content), (foo, content)]
    chooseRep hasreps [TypeHtml] >>= (@?= (TypeHtml, content))
    chooseRep hasreps [foo, bar] >>= (@?= (foo, content))
    chooseRep hasreps [bar, foo] >>= (@?= (foo, content))
    chooseRep hasreps [bar]      >>= (@?= (TypeHtml, content))

caseChooseRepTemplate :: Assertion
caseChooseRepTemplate = do
    let temp = newSTMP "foo:$o.foo$, bar:$o.bar$"
        ho = toHtmlObject [ ("foo", toHtmlObject "<fooval>")
                          , ("bar", toHtmlObject ["bar1", "bar2"])
                          ]
        hasreps = Template temp "o" ho $ return []
        res1 = cs "foo:&lt;fooval&gt;, bar:bar1bar2"
        res2 = cs $ "{\"bar\":[\"bar1\",\"bar2\"]," ++
                    "\"foo\":\"&lt;fooval&gt;\"}"
    chooseRep hasreps [TypeHtml]           >>= (@?= (TypeHtml, res1))
    chooseRep hasreps [TypeJson]           >>= (@?= (TypeJson, res2))
    chooseRep hasreps [TypeHtml, TypeJson] >>= (@?= (TypeHtml, res1))
    chooseRep hasreps [TypeJson, TypeHtml] >>= (@?= (TypeJson, res2))

caseChooseRepTemplateFile :: Assertion
caseChooseRepTemplateFile = do
    let temp = "Test/rep.st"
        ho = toHtmlObject [ ("foo", toHtmlObject "<fooval>")
                          , ("bar", toHtmlObject ["bar1", "bar2"])
                          ]
        hasreps = TemplateFile temp ho
        res1 = cs "foo:&lt;fooval&gt;, bar:bar1bar2"
        res2 = cs $ "{\"bar\":[\"bar1\",\"bar2\"]," ++
                    "\"foo\":\"&lt;fooval&gt;\"}"
    chooseRep hasreps [TypeHtml]           >>= (@?= (TypeHtml, res1))
    chooseRep hasreps [TypeJson]           >>= (@?= (TypeJson, res2))
    chooseRep hasreps [TypeHtml, TypeJson] >>= (@?= (TypeHtml, res1))
    chooseRep hasreps [TypeJson, TypeHtml] >>= (@?= (TypeJson, res2))

testSuite :: Test
testSuite = testGroup "Yesod.Rep"
    [ testCase "caseChooseRep HtmlObject" caseChooseRepHO
    , testCase "caseChooseRep raw" caseChooseRepRaw
    , testCase "caseChooseRep Template" caseChooseRepTemplate
    , testCase "caseChooseRep TemplateFile" caseChooseRepTemplateFile
    ]
#endif
