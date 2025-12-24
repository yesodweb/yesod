{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns#-}

import Test.Hspec
import Test.HUnit ((@?=))
import Data.Text (Text)
import Yesod.Routes.Class hiding (Route)
import Yesod.Routes.Parse (parseTypeTree, TypeTree (..))
import Yesod.Core
import Yesod.Routes.Overlap (findOverlapNames)
import Yesod.Routes.TH hiding (Dispatch)
import Hierarchy
import qualified Data.Set as Set
import qualified Route.FallthroughSpec as FallthroughSpec
import qualified Route.RenderRouteSpec as RenderRouteSpec
import qualified Data.Text as Text

data MyApp = MyApp

data MySub = MySub
instance RenderRoute MySub where
    data
        Route
        MySub = MySubRoute ([Text], [(Text, Text)])
        deriving (Show, Eq, Read)
    renderRoute (MySubRoute x) = x
instance ParseRoute MySub where
    parseRoute = Just . MySubRoute

getMySub :: MyApp -> MySub
getMySub MyApp = MySub

data MySubParam = MySubParam Int
instance RenderRoute MySubParam where
    data
        Route
        MySubParam = ParamRoute Char
        deriving (Show, Eq, Read)
    renderRoute (ParamRoute x) = ([Text.singleton x], [])
instance ParseRoute MySubParam where
    parseRoute ([Text.unpack -> [x]], _) = Just $ ParamRoute x
    parseRoute _ = Nothing

getMySubParam :: MyApp -> Int -> MySubParam
getMySubParam _ = MySubParam

do
    texts <- pure "[Text]"
    let resLeaves = map ResourceLeaf
            [ Resource "RootR" [] (Methods Nothing ["GET"]) ["foo", "bar"] True
            , Resource "BlogPostR" [Static "blog", Dynamic "Text"] (Methods Nothing ["GET", "POST"]) [] True
            , Resource "WikiR" [Static "wiki"] (Methods (Just texts) []) [] True
            , Resource "SubsiteR" [Static "subsite"] (Subsite "MySub" "getMySub") [] True
            , Resource "SubparamR" [Static "subparam", Dynamic "Int"] (Subsite "MySubParam" "getMySubParam") [] True
            ]
        resParent = ResourceParent
            "ParentR"
            True
            [ Static "foo"
            , Dynamic "Text"
            ]
            [ ResourceLeaf $ Resource "ChildR" [] (Methods Nothing ["GET"]) ["child"] True
            ]
        ress = resParent : resLeaves
    mkYesod "MyApp" ress

instance YesodSubDispatch MySub MyApp where
    yesodSubDispatch _yre = undefined

instance YesodSubDispatch MySubParam MyApp where
    yesodSubDispatch _yre = undefined

instance Yesod MyApp where
    messageLoggerSource = mempty

main :: IO ()
main = hspec $ do
    describe "Route.FallthroughSpec" FallthroughSpec.spec
    describe "Route.RenderRouteSpec" RenderRouteSpec.spec
    describe "RenderRoute instance" $ do
        it "renders root correctly" $ renderRoute RootR @?= ([], [])
        it "renders blog post correctly" $ renderRoute (BlogPostR $ Text.pack "foo") @?= (map Text.pack ["blog", "foo"], [])
        it "renders wiki correctly" $ renderRoute (WikiR $ map Text.pack ["foo", "bar"]) @?= (map Text.pack ["wiki", "foo", "bar"], [])
        it "renders subsite correctly" $ renderRoute (SubsiteR $ MySubRoute (map Text.pack ["foo", "bar"], [(Text.pack "baz", Text.pack "bin")]))
            @?= (map Text.pack ["subsite", "foo", "bar"], [(Text.pack "baz", Text.pack "bin")])
        it "renders subsite param correctly" $ renderRoute (SubparamR 6 $ ParamRoute 'c')
            @?= (map Text.pack ["subparam", "6", "c"], [])


    describe "route parsing" $ do
        it "subsites work" $ do
            parseRoute ([Text.pack "subsite", Text.pack "foo"], [(Text.pack "bar", Text.pack "baz")]) @?=
                Just (SubsiteR $ MySubRoute ([Text.pack "foo"], [(Text.pack "bar", Text.pack "baz")]))

    describe "routing table parsing" $ do
        it "recognizes trailing backslashes as line continuation directives" $ do
            let routes :: [ResourceTree String]
                routes = $(parseRoutesFile "test/fixtures/routes_with_line_continuations.yesodroutes")
            length routes @?= 3

    describe "overlap checking" $ do
        it "catches overlapping statics" $ do
            let routes :: [ResourceTree String]
                routes = [parseRoutesNoCheck|
/foo Foo1
/foo Foo2
|]
            findOverlapNames routes @?= [("Foo1", "Foo2")]
        it "catches overlapping dynamics" $ do
            let routes :: [ResourceTree String]
                routes = [parseRoutesNoCheck|
/#Int Foo1
/#String Foo2
|]
            findOverlapNames routes @?= [("Foo1", "Foo2")]
        it "catches overlapping statics and dynamics" $ do
            let routes :: [ResourceTree String]
                routes = [parseRoutesNoCheck|
/foo Foo1
/#String Foo2
|]
            findOverlapNames routes @?= [("Foo1", "Foo2")]
        it "catches overlapping multi" $ do
            let routes :: [ResourceTree String]
                routes = [parseRoutesNoCheck|
/foo Foo1
/##*Strings Foo2
|]
            findOverlapNames routes @?= [("Foo1", "Foo2")]
        it "catches overlapping subsite" $ do
            let routes :: [ResourceTree String]
                routes = [parseRoutesNoCheck|
/foo Foo1
/foo Foo2 Subsite getSubsite
|]
            findOverlapNames routes @?= [("Foo1", "Foo2")]
        it "no false positives" $ do
            let routes :: [ResourceTree String]
                routes = [parseRoutesNoCheck|
/foo Foo1
/bar/#String Foo2
|]
            findOverlapNames routes @?= []
        it "obeys ignore rules" $ do
            let routes :: [ResourceTree String]
                routes = [parseRoutesNoCheck|
/foo Foo1
/#!String Foo2
/!foo Foo3
|]
            findOverlapNames routes @?= []
        it "obeys multipiece ignore rules #779" $ do
            let routes :: [ResourceTree String]
                routes = [parseRoutesNoCheck|
/foo Foo1
/+![String] Foo2
|]
            findOverlapNames routes @?= []
        it "ignore rules for entire route #779" $ do
            let routes :: [ResourceTree String]
                routes = [parseRoutesNoCheck|
/foo Foo1
!/+[String] Foo2
!/#String Foo3
!/foo Foo4
|]
            findOverlapNames routes @?= []
        it "ignore rules for hierarchy" $ do
            let routes :: [ResourceTree String]
                routes = [parseRoutesNoCheck|
/+[String] Foo1
!/foo Foo2:
    /foo Foo3
/foo Foo4:
    /!#foo Foo5
|]
            findOverlapNames routes @?= []
        it "proper boolean logic" $ do
            let routes = [parseRoutesNoCheck|
/foo/bar Foo1
/foo/baz Foo2
/bar/baz Foo3
|]
            findOverlapNames routes @?= []
    describe "routeAttrs" $ do
        it "works" $ do
            routeAttrs RootR @?= Set.fromList [Text.pack "foo", Text.pack "bar"]
        it "hierarchy" $ do
            routeAttrs (ParentR (Text.pack "ignored") ChildR) @?= Set.singleton (Text.pack "child")
    hierarchy
    describe "parseRouteType" $ do
        let success s t = it s $ parseTypeTree s @?= Just t
            failure s = it s $ parseTypeTree s @?= Nothing
        success "Int" $ TTTerm "Int"
        success "(Int)" $ TTTerm "Int"
        failure "(Int"
        failure "(Int))"
        failure "[Int"
        failure "[Int]]"
        success "[Int]" $ TTList $ TTTerm "Int"
        success "Foo-Bar" $ TTApp (TTTerm "Foo") (TTTerm "Bar")
        success "Foo-Bar-Baz" $ TTApp (TTTerm "Foo") (TTTerm "Bar") `TTApp` TTTerm "Baz"
        success "Foo Bar" $ TTApp (TTTerm "Foo") (TTTerm "Bar")
        success "Foo Bar Baz" $ TTApp (TTTerm "Foo") (TTTerm "Bar") `TTApp` TTTerm "Baz"

getRootR :: HandlerFor site Text
getRootR = pure $ Text.pack "this is the root"

getBlogPostR :: Text -> HandlerFor site String
getBlogPostR t = pure $ "some blog post: " ++ Text.unpack t

postBlogPostR :: Text -> HandlerFor site Text
postBlogPostR t = pure $ Text.pack $ "POST some blog post: " ++ Text.unpack t

handleWikiR :: [Text] -> HandlerFor site String
handleWikiR ts = pure $ "the wiki: " ++ show ts

getChildR :: Text -> HandlerFor site Text
getChildR = pure
