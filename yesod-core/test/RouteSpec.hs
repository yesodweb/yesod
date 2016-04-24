{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns#-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -ddump-splices #-}
import Test.Hspec
import Test.HUnit ((@?=))
import Data.Text (Text, pack, unpack, singleton)
import Yesod.Routes.Class hiding (Route)
import qualified Yesod.Routes.Class as YRC
import Yesod.Routes.Parse (parseRoutesNoCheck, parseTypeTree, TypeTree (..))
import Yesod.Routes.Overlap (findOverlapNames)
import Yesod.Routes.TH hiding (Dispatch)
import Language.Haskell.TH.Syntax
import Hierarchy
import qualified Data.ByteString.Char8 as S8
import qualified Data.Set as Set

data MyApp = MyApp

data MySub = MySub
instance RenderRoute MySub where
    data
#if MIN_VERSION_base(4,5,0)
        Route
#else
        YRC.Route
#endif
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
#if MIN_VERSION_base(4,5,0)
        Route
#else
        YRC.Route
#endif
        MySubParam = ParamRoute Char
        deriving (Show, Eq, Read)
    renderRoute (ParamRoute x) = ([singleton x], [])
instance ParseRoute MySubParam where
    parseRoute ([unpack -> [x]], _) = Just $ ParamRoute x
    parseRoute _ = Nothing

getMySubParam :: MyApp -> Int -> MySubParam
getMySubParam _ = MySubParam

do
    texts <- [t|[Text]|]
    let resLeaves = map ResourceLeaf
            [ Resource "RootR" [] (Methods Nothing ["GET"]) ["foo", "bar"] True
            , Resource "BlogPostR" [Static "blog", Dynamic $ ConT ''Text] (Methods Nothing ["GET", "POST"]) [] True
            , Resource "WikiR" [Static "wiki"] (Methods (Just texts) []) [] True
            , Resource "SubsiteR" [Static "subsite"] (Subsite (ConT ''MySub) "getMySub") [] True
            , Resource "SubparamR" [Static "subparam", Dynamic $ ConT ''Int] (Subsite (ConT ''MySubParam) "getMySubParam") [] True
            ]
        resParent = ResourceParent
            "ParentR"
            True
            [ Static "foo"
            , Dynamic $ ConT ''Text
            ]
            [ ResourceLeaf $ Resource "ChildR" [] (Methods Nothing ["GET"]) ["child"] True
            ]
        ress = resParent : resLeaves
    rrinst <- mkRenderRouteInstance (ConT ''MyApp) ress
    rainst <- mkRouteAttrsInstance (ConT ''MyApp) ress
    prinst <- mkParseRouteInstance (ConT ''MyApp) ress
    dispatch <- mkDispatchClause MkDispatchSettings
        { mdsRunHandler = [|runHandler|]
        , mdsSubDispatcher = [|subDispatch dispatcher|]
        , mdsGetPathInfo = [|fst|]
        , mdsMethod = [|snd|]
        , mdsSetPathInfo = [|\p (_, m) -> (p, m)|]
        , mds404 = [|pack "404"|]
        , mds405 = [|pack "405"|]
        , mdsGetHandler = defaultGetHandler
        , mdsUnwrapper = return
        } ress
    return
#if MIN_VERSION_template_haskell(2,11,0)
        $ InstanceD Nothing
#else
        $ InstanceD
#endif
            []
            (ConT ''Dispatcher
                `AppT` ConT ''MyApp
                `AppT` ConT ''MyApp)
            [FunD (mkName "dispatcher") [dispatch]]
        : prinst
        : rainst
        : rrinst

instance Dispatcher MySub master where
    dispatcher env (pieces, _method) =
        ( pack $ "subsite: " ++ show pieces
        , Just $ envToMaster env route
        )
      where
        route = MySubRoute (pieces, [])

instance Dispatcher MySubParam master where
    dispatcher env (pieces, method) =
        case map unpack pieces of
            [[c]] ->
                let route = ParamRoute c
                    toMaster = envToMaster env
                    MySubParam i = envSub env
                 in ( pack $ "subparam " ++ show i ++ ' ' : [c]
                    , Just $ toMaster route
                    )
            _ -> (pack "404", Nothing)

{-
thDispatchAlias
    :: (master ~ MyApp, sub ~ MyApp, handler ~ String, app ~ (String, Maybe (YRC.Route MyApp)))
    => master
    -> sub
    -> (YRC.Route sub -> YRC.Route master)
    -> app -- ^ 404 page
    -> handler -- ^ 405 page
    -> Text -- ^ method
    -> [Text]
    -> app
--thDispatchAlias = thDispatch
thDispatchAlias master sub toMaster app404 handler405 method0 pieces0 =
    case dispatch pieces0 of
        Just f -> f master sub toMaster app404 handler405 method0
        Nothing -> app404
  where
    dispatch = toDispatch
        [ Route [] False $ \pieces ->
            case pieces of
                [] -> do
                    Just $ \master' sub' toMaster' _app404' handler405' method ->
                        let handler =
                                case Map.lookup method methodsRootR of
                                    Just f -> f
                                    Nothing -> handler405'
                         in runHandler handler master' sub' RootR toMaster'
                _ -> error "Invariant violated"
        , Route [D.Static "blog", D.Dynamic] False $ \pieces ->
            case pieces of
                [_, x2] -> do
                    y2 <- fromPathPiece x2
                    Just $ \master' sub' toMaster' _app404' handler405' method ->
                        let handler =
                                case Map.lookup method methodsBlogPostR of
                                    Just f -> f y2
                                    Nothing -> handler405'
                         in runHandler handler master' sub' (BlogPostR y2) toMaster'
                _ -> error "Invariant violated"
        , Route [D.Static "wiki"] True $ \pieces ->
            case pieces of
                _:x2 -> do
                    y2 <- fromPathMultiPiece x2
                    Just $ \master' sub' toMaster' _app404' _handler405' _method ->
                        let handler = handleWikiR y2
                         in runHandler handler master' sub' (WikiR y2) toMaster'
                _ -> error "Invariant violated"
        , Route [D.Static "subsite"] True $ \pieces ->
            case pieces of
                _:x2 -> do
                    Just $ \master' sub' toMaster' app404' handler405' method ->
                        dispatcher master' (getMySub sub') (toMaster' . SubsiteR) app404' handler405' method x2
                _ -> error "Invariant violated"
        , Route [D.Static "subparam", D.Dynamic] True $ \pieces ->
            case pieces of
                _:x2:x3 -> do
                    y2 <- fromPathPiece x2
                    Just $ \master' sub' toMaster' app404' handler405' method ->
                        dispatcher master' (getMySubParam sub' y2) (toMaster' . SubparamR y2) app404' handler405' method x3
                _ -> error "Invariant violated"
        ]
    methodsRootR = Map.fromList [("GET", getRootR)]
    methodsBlogPostR = Map.fromList [("GET", getBlogPostR), ("POST", postBlogPostR)]
-}

main :: IO ()
main = hspec $ do
    describe "RenderRoute instance" $ do
        it "renders root correctly" $ renderRoute RootR @?= ([], [])
        it "renders blog post correctly" $ renderRoute (BlogPostR $ pack "foo") @?= (map pack ["blog", "foo"], [])
        it "renders wiki correctly" $ renderRoute (WikiR $ map pack ["foo", "bar"]) @?= (map pack ["wiki", "foo", "bar"], [])
        it "renders subsite correctly" $ renderRoute (SubsiteR $ MySubRoute (map pack ["foo", "bar"], [(pack "baz", pack "bin")]))
            @?= (map pack ["subsite", "foo", "bar"], [(pack "baz", pack "bin")])
        it "renders subsite param correctly" $ renderRoute (SubparamR 6 $ ParamRoute 'c')
            @?= (map pack ["subparam", "6", "c"], [])

    describe "thDispatch" $ do
        let disp m ps = dispatcher
                (Env
                    { envToMaster = id
                    , envMaster = MyApp
                    , envSub = MyApp
                    })
                (map pack ps, S8.pack m)
        it "routes to root" $ disp "GET" [] @?= (pack "this is the root", Just RootR)
        it "POST root is 405" $ disp "POST" [] @?= (pack "405", Just RootR)
        it "invalid page is a 404" $ disp "GET" ["not-found"] @?= (pack "404", Nothing :: Maybe (YRC.Route MyApp))
        it "routes to blog post" $ disp "GET" ["blog", "somepost"]
            @?= (pack "some blog post: somepost", Just $ BlogPostR $ pack "somepost")
        it "routes to blog post, POST method" $ disp "POST" ["blog", "somepost2"]
            @?= (pack "POST some blog post: somepost2", Just $ BlogPostR $ pack "somepost2")
        it "routes to wiki" $ disp "DELETE" ["wiki", "foo", "bar"]
            @?= (pack "the wiki: [\"foo\",\"bar\"]", Just $ WikiR $ map pack ["foo", "bar"])
        it "routes to subsite" $ disp "PUT" ["subsite", "baz"]
            @?= (pack "subsite: [\"baz\"]", Just $ SubsiteR $ MySubRoute ([pack "baz"], []))
        it "routes to subparam" $ disp "PUT" ["subparam", "6", "q"]
            @?= (pack "subparam 6 q", Just $ SubparamR 6 $ ParamRoute 'q')

    describe "parsing" $ do
        it "subsites work" $ do
            parseRoute ([pack "subsite", pack "foo"], [(pack "bar", pack "baz")]) @?=
                Just (SubsiteR $ MySubRoute ([pack "foo"], [(pack "bar", pack "baz")]))

    describe "overlap checking" $ do
        it "catches overlapping statics" $ do
            let routes = [parseRoutesNoCheck|
/foo Foo1
/foo Foo2
|]
            findOverlapNames routes @?= [("Foo1", "Foo2")]
        it "catches overlapping dynamics" $ do
            let routes = [parseRoutesNoCheck|
/#Int Foo1
/#String Foo2
|]
            findOverlapNames routes @?= [("Foo1", "Foo2")]
        it "catches overlapping statics and dynamics" $ do
            let routes = [parseRoutesNoCheck|
/foo Foo1
/#String Foo2
|]
            findOverlapNames routes @?= [("Foo1", "Foo2")]
        it "catches overlapping multi" $ do
            let routes = [parseRoutesNoCheck|
/foo Foo1
/##*Strings Foo2
|]
            findOverlapNames routes @?= [("Foo1", "Foo2")]
        it "catches overlapping subsite" $ do
            let routes = [parseRoutesNoCheck|
/foo Foo1
/foo Foo2 Subsite getSubsite
|]
            findOverlapNames routes @?= [("Foo1", "Foo2")]
        it "no false positives" $ do
            let routes = [parseRoutesNoCheck|
/foo Foo1
/bar/#String Foo2
|]
            findOverlapNames routes @?= []
        it "obeys ignore rules" $ do
            let routes = [parseRoutesNoCheck|
/foo Foo1
/#!String Foo2
/!foo Foo3
|]
            findOverlapNames routes @?= []
        it "obeys multipiece ignore rules #779" $ do
            let routes = [parseRoutesNoCheck|
/foo Foo1
/+![String] Foo2
|]
            findOverlapNames routes @?= []
        it "ignore rules for entire route #779" $ do
            let routes = [parseRoutesNoCheck|
/foo Foo1
!/+[String] Foo2
!/#String Foo3
!/foo Foo4
|]
            findOverlapNames routes @?= []
        it "ignore rules for hierarchy" $ do
            let routes = [parseRoutesNoCheck|
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
            routeAttrs RootR @?= Set.fromList [pack "foo", pack "bar"]
        it "hierarchy" $ do
            routeAttrs (ParentR (pack "ignored") ChildR) @?= Set.singleton (pack "child")
    hierarchy
    describe "parseRouteTyoe" $ do
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

getRootR :: Text
getRootR = pack "this is the root"

getBlogPostR :: Text -> String
getBlogPostR t = "some blog post: " ++ unpack t

postBlogPostR :: Text -> Text
postBlogPostR t = pack $ "POST some blog post: " ++ unpack t

handleWikiR :: [Text] -> String
handleWikiR ts = "the wiki: " ++ show ts

getChildR :: Text -> Text
getChildR = id
