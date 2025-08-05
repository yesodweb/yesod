# Nested Route Discovery

We have almost 2,000 total routes in our codebase now, with 1,100 of them on the top-level.
We have been leveraging nested routes to organize the routes and make certain things easier - namely, reducing the overall size of the `Route WebApp` which makes pattern matching more efficient in compilation.

However, compiling `mkYesodData` for our application is a large bottleneck.
Compilation takes a significant amount of time and much work must be redone, even when most of the routing information could have been saved.
One thing that would significantly help is the ability to separate out the sub-route datatypes, generate the instance for these separately, and refer to them elsewhere.

Another thing that would help is having a finer grained `YesodDispatch` facility - allowing us to, in tests, specify more precisely which parts of the route structure we actually need to deal with.
Currently, we need almost 7k modules in order to compile a test that refers to `YesodDispatch`, since this brings in the transitive dependencies of every web handler.
With a finer grained `YesodDispatch`, able to refer to route fragments, we'd be able to avoid depending on more of the website than we really need to.
We expect this to yield significant benefits when we move more testing infrastructure to buck2 - right now, changing anything used by any part of the web app would require running *all* tests that exercise *anything* in the webapp!
With more granular `YesodDispatch`, we gain the ability to only run tests on parts of the app that actually use the route structure in question.

# Design Considerations

The code generation creates instances of several classes:

1. `RenderRoute`
2. `ParseRoute`
3. `RouteAttrs`
4. `YesodDispatch`

We need another class for each of these which can accept a nested subroute.

This design *must* be backwards compatible, and so the design should only generate sub-instances if they don't already exist.

The design must support partiality - that is, we should be able to *selectively* make separate sub-instances out of a composite `yesodroutes` file or `[ResourceTree String]`.

## `RenderRoute`

`RenderRoute` is where we define the actual `Route a` datatype and a function `renderRoute :: Route a -> ([Text], [(Text, Text)])`.
The function that creates the instance is [`mkRenderRouteInstanceOpts`](https://github.com/yesodweb/yesod/blob/2570a91a76bce15fa6a28f48d442b5ad4e764c35/yesod-core/src/Yesod/Routes/TH/RenderRoute.hs?plain=1#L220-L251).
The clauses are defined by [`mkRenderRouteClauses`](https://github.com/yesodweb/yesod/blob/2570a91a76bce15fa6a28f48d442b5ad4e764c35/yesod-core/src/Yesod/Routes/TH/RenderRoute.hs?plain=1#L129-L157), and the `ResourceParent` constructor here is what's actually used by the nested route construction.
The code is instructive in what is generated:

```haskell
    go (ResourceParent name _check pieces children) = do
        let cnt = length $ filter isDynamic pieces
        dyns <- replicateM cnt $ newName "dyn"
        child <- newName "child"
        let pat = conPCompat (mkName name) $ map VarP $ dyns ++ [child]

        pack' <- [|pack|]
        tsp <- [|toPathPiece|]
        let piecesSingle = mkPieces (AppE pack' . LitE . StringL) tsp pieces dyns

        childRender <- newName "childRender"
        let rr = VarE childRender
        childClauses <- mkRenderRouteClauses children

        a <- newName "a"
        b <- newName "b"

        colon <- [|(:)|]
        let cons y ys = InfixE (Just y) colon (Just ys)
        let pieces' = foldr cons (VarE a) piecesSingle

        let body = LamE [TupP [VarP a, VarP b]] (TupE
#if MIN_VERSION_template_haskell(2,16,0)
                                                  $ map Just
#endif
                                                  [pieces', VarE b]
                                                ) `AppE` (rr `AppE` VarE child)

        return $ Clause [pat] (NormalB body) [FunD childRender childClauses]
```

Interestingly, `mkRenderRouteClauses` is called *directly* on the `children`, meaning that our child class can differ in only avoiding a `Route a` declaration.
Translating the above `Clause` into some pseudocode, let's say we have:

```
/foo/#{OrganizationId}: FooR
    /           FooIndexR GET POST
    /#{FooId}   FooShowR  GET POST
```

This would give us a `[ResourceTree String]` like:

```haskell
[ ResourceParent "FooR" _ [Static "foo", Dynamic "OrgId"] 
    [ ResourceLeaf Resource 
        { resourceName = "FooIndexR"
        , resourcePieces = []
        , resourceDispatch = Methods 
              { methodsMulti = Nothing
              , methodsMethods = ["GET", "POST"]
              }
        }
    , ResourceLeaf Resource
        { resourceName "FooShowR"
        , resourcePieces = [Dynamic "FooId"]
        , resourceDispatch = Methods
            { methodsMulti = Nothing
            , methodsMethods = ["GET", "POST"]
            }
        }
    ]
]
```

And this translates to this clause:

```haskell
    renderRoute (FooR orgId children) = 
        (\(a, b) ->
            ( "foo" : toPathPiece orgId : a
            , b
            )
        ) (childRender children)
      where
        childRender FooIndexR = ([], [])
        childRender (FooShowR fooId) = ([toPathPiece fooId], [])
```

Now: we will need to reflect on the presence of an instance of `RenderRouteNested a`.
If one exists, we use it instead of `childRender`.
Otherwise, we generate code as normal.

## `ParseRoute`

`parseRoute` is a mirror of `renderRoute`:

```haskell
class (RenderRoute a) => ParseRoute a where
    parseRoute :: ([Text], [(Text, Text)]) -> Maybe (Route a)
```

The `TemplateHaskell` that generates it is called [`mkParseRouteInstance`](https://github.com/yesodweb/yesod/blob/2570a91a76bce15fa6a28f48d442b5ad4e764c35/yesod-core/src/Yesod/Routes/TH/ParseRoute.hs?plain=1#L14-L45), and is a little weird.
First we `mkDispatchClause`.
Then we create a `fixer` defined as `fixer <- [| (\f x -> f () x) :: type sig elided |]`.
The whole `parseRoute` definition is (rendering):

```haskell
instance ParseRoute $(site) where
    parseRoute = 
        (\f x -> f () x :: (() -> ([Text], [(Text, Text)]) -> Maybe (Route a)) -> ([Text], [(Text, Text)]) -> Maybe (Route a)) 
        helper
      where
        helper = $(mkDispatchClause MkDispatchSettings {..} (map removeMethods ress))
```

The secret is in the specific [`MkDispatchSettings`](https://github.com/yesodweb/yesod/blob/2570a91a76bce15fa6a28f48d442b5ad4e764c35/yesod-core/src/Yesod/Routes/TH/ParseRoute.hs?plain=1#L16-L28), along with knowing that we do `removeMethods` on the resources.
This means we run into [this code branch](https://github.com/yesodweb/yesod/blob/2570a91a76bce15fa6a28f48d442b5ad4e764c35/yesod-core/src/Yesod/Routes/TH/Dispatch.hs?plain=1#L156-L158) - [`mkRunExp Nothing`](https://github.com/yesodweb/yesod/blob/2570a91a76bce15fa6a28f48d442b5ad4e764c35/yesod-core/src/Yesod/Routes/TH/Dispatch.hs?plain=1#L156-L158):

```haskell
                        mkRunExp mmethod = do
                            runHandlerE <- mdsRunHandler
                            handlerE' <- mdsGetHandler mmethod name
                            handlerE <- mdsUnwrapper $ foldl' AppE handlerE' allDyns
                            return $ runHandlerE
                                `AppE` handlerE
                                `AppE` envExp
                                `AppE` jroute
                                `AppE` reqExp
```

This becomes [`chooseMethod` here](https://github.com/yesodweb/yesod/blob/2570a91a76bce15fa6a28f48d442b5ad4e764c35/yesod-core/src/Yesod/Routes/TH/Dispatch.hs?plain=1#L118-L123):

```haskell
        (chooseMethod, finalPat) <- handleDispatch dispatch dyns

        return $ Clause
            [mkPathPat finalPat pats]
            (NormalB chooseMethod)
            []
```

Huh. OK.
And, of course, to [finish up `mkDispatchClause`](https://github.com/yesodweb/yesod/blob/2570a91a76bce15fa6a28f48d442b5ad4e764c35/yesod-core/src/Yesod/Routes/TH/Dispatch.hs?plain=1#L71-L74), we have this:

```haskell
    return $ Clause
        [VarP envName, VarP reqName]
        (NormalB $ helperE `AppE` pathInfo)
        [FunD helperName $ clauses ++ [clause404']]
```

Plugging that in in `helper`,

```haskell
instance ParseRoute $(site) where
    parseRoute = 
        (\f x -> f () x :: (() -> ([Text], [(Text, Text)]) -> Maybe (Route a)) -> ([Text], [(Text, Text)]) -> Maybe (Route a)) 
        helper
  where
    helper env req = 
        $(mdsRunHandler) 
            $(mdsGetHandler Nothing name >>= \handlerE -> mdsUnwrapper (foldl' AppE handlerE allDyns))
            $(envExp)
            $(jroute)
            $(reqExp)
```

Now, in our case, we have `mdsRunHandler = [| \_ _ x _ -> x |]`. So we can simplify our code:

```haskell
instance ParseRoute $(site) where
    parseRoute = 
        (\f x -> f () x :: (() -> ([Text], [(Text, Text)]) -> Maybe (Route a)) -> ([Text], [(Text, Text)]) -> Maybe (Route a)) 
        helper
  where
    helper env req = 
        $(mdsRunHandler) 
            -- $(mdsGetHandler Nothing name >>= \handlerE -> mdsUnwrapper (foldl' AppE handlerE allDyns))
            -- $(envExp)
            $(jroute)
            -- $(reqExp)
```

So what is [`jroute`](https://github.com/yesodweb/yesod/blob/2570a91a76bce15fa6a28f48d442b5ad4e764c35/yesod-core/src/Yesod/Routes/TH/Dispatch.hs?plain=1#L125-L144)?

```haskell
        handleDispatch :: Dispatch a -> [Exp] -> Q (Exp, Pat)
        handleDispatch dispatch' dyns =
            case dispatch' of
                Methods multi methods -> do
                    (finalPat, mfinalE) <-
                        case multi of
                            Nothing -> return (conPCompat '[] [], Nothing)
                            Just _ -> do
                                multiName <- newName "multi"
                                let pat = ViewP (VarE 'fromPathMultiPiece)
                                                (conPCompat 'Just [VarP multiName])
                                return (pat, Just $ VarE multiName)

                    let dynsMulti =
                            case mfinalE of
                                Nothing -> dyns
                                Just e -> dyns ++ [e]
                        route' = foldl' AppE (ConE (mkName name)) dynsMulti
                        route = foldr AppE route' extraCons
                        jroute = ConE 'Just `AppE` route
```

This is a weirdly complicated bit of code reuse.
But I think the overall gist here is that we have a - uh - a big ass pattern match, and one of the things that we provide in is the `jroute`, which is the actual `Route a` that we are constructing?
And so if the patterns match, we call `mdsRunHandler`, one of which is an argument for the `Route a`, and then we *return that*.
If there's no match, we return `Nothing`.

OK.
Wild.
This may be harder to work with.

This is the [actual code for a nest point](https://github.com/yesodweb/yesod/blob/2570a91a76bce15fa6a28f48d442b5ad4e764c35/yesod-core/src/Yesod/Routes/TH/Dispatch.hs?plain=1#L95-L114):

```haskell
    go :: SDC -> ResourceTree a -> Q Clause
    go sdc (ResourceParent name _check pieces children) = do
        (pats, dyns) <- handlePieces pieces
        let sdc' = sdc
                { extraParams = extraParams sdc ++ dyns
                , extraCons = extraCons sdc ++ [mkCon name dyns]
                }
        childClauses <- mapM (go sdc') children

        restName <- newName "rest"
        let restE = VarE restName
            restP = VarP restName

        helperName <- newName $ "helper" ++ name
        let helperE = VarE helperName

        return $ Clause
            [mkPathPat restP pats]
            (NormalB $ helperE `AppE` restE)
            [FunD helperName $ childClauses ++ [clause404 sdc]]
```

So we actually *do* have  reasonable "recursion point" - we `go` on `children` with the `sdc` with extra params and extra constructors set.

I think the way forward here is to reuse this logic. Filter the `[ResourceTree a]` until you find one with a `ResourceParent "YourSubrouteR" _ _ _`, then call this on that.
The same customization applies.
We *do* need to have the `extraParams` and `extraCons` threaded through, so the `ParseRouteNested` needs to account for these values.

Although, we do not need the prefix information to construct an `x :: AdminR` value, that we then apply to `AdminR :: AdminR -> Route app`.
So maybe this is actually much easier: we just strip out the "prefix" that's been successfully parsed out?

# OK, dispatch code is nuts

This code is quite hard to work with!
Indirection makes it tricky to follow exactly what is going on.

So, right now, the `NestInner` test isn't working, because we're generating the code that only has the "Nothing" case covered.
This is specifically happening in the triply nested case, repreoduced here:

```
/nest/ NestR !NestingAttr:

  /spaces      SpacedR   GET !NonNested

  /nest2 Nest2:
    /           GetPostR  GET POST
    /get        Get2      GET
    /post       Post2         POST
    /nest-inner NestInner:
        /       NestInnerIndexR GET
```

Datatype generation works fine.
And parsing appears to work fine for *some* subroutes.
So why is this not working?

We appear to be failing to generate a `Clause` for this.

THe problem is that in a multi-nested step, we were filtering it out.
So `mnrs` would detect a match in the string we want at `NestInner`, and then it would parse the route correctly.
But we go up in the recursion to `Nest2`.
So now I think we need to manage the `sdc` better...

OK, now all the `Hierarchy` tests pass.
Let's run the rest of the tests and see how things are doing...

All tests pass, great.
Now, it's time for dispatch.

# Dispatch

The `Yesod.Routes.TH.Dispatch` creates an instance of `YesodDispatch`.
The test `test/YesodCoreTest/Reps.hs` generates the following instance:

```haskell
    instance YesodDispatch App where
      yesodDispatch env req
        = helper (pathInfo req)
        where
            helper []
              = case requestMethod req of
                  "GET" -> yesodRunner getHomeR env (Just HomeR) req
                  _ -> yesodRunner
                         (void badMethod) env (Just HomeR) req
            helper ((:) "json" [])
              = case requestMethod req of
                  "GET" -> yesodRunner getJsonR env (Just JsonR) req
                  _ -> yesodRunner
                         (void badMethod) env (Just JsonR) req
            helper
              ((:) "parent" ((:) (fromPathPiece -> Just dyn) rest))
              = helperParentR rest
              where
                  helperParentR
                    ((:) (fromPathPiece -> Just dyn') ((:) "child" []))
                    = yesodRunner
                        (handleChildR dyn dyn') env
                        (Just (ParentR dyn (ChildR dyn'))) req
                  helperParentR _
                    = yesodRunner
                        (void notFound) env Nothing req
            helper _
              = yesodRunner
                  (void notFound) env Nothing req
```

So we call [`yesodRunner`](https://github.com/yesodweb/yesod/blob/c27d5116445453470b88287eef18a4b7fda96135/yesod-core/src/Yesod/Core/Internal/Run.hs?plain=1#L308-L312), which returns a full WAI `Application` given a `HandlerFor site res` and a `Maybe (Route site)`.

In `YesodDispatchNested`, in order to call `yesodRunner`, we will need to propagate the arguments from the path (`dyn`, defined in the signature on `helperParentR`), and maybe also a function `sub -> Route site`?

A function `sub -> Route site` would require that the *callsite* of the `yesodDispatchNested` function to know about `Route site` and construct it manually, which isn't really great.

So, perhaps, instead of `yesodRunner`, we return the `HandlerFor site res`.
We need to pass in a potentially dynamic "parent args" value.
So, possible class design,

```haskell
class YesodNestedDispatch parent nest | nest -> parent where
    type ParentArgs nest :: Type
    type ParentArgs = ()
    yesodNestedDispatch 
        :: (ToTypedContent res, Yesod parent) 
        => ParentArgs nest
        -> YesodRunnerEnv parent
        -> Request
        -- ^ original request:
        -> [Text]
        -- ^ remaining path fragments
        -> HandlerFor site res
```

But I don't think this quite works either.
Because we're not going to be returning the *same* thing, we're returning potentially *different* things.
We could call `ToTypedContent` on it, since that's what the calling function does anyway - calls `runHandler`, which calls `basicRunHandler`, which [calls `toTypedContent`](https://github.com/yesodweb/yesod/blob/c27d5116445453470b88287eef18a4b7fda96135/yesod-core/src/Yesod/Core/Internal/Run.hs?plain=1#L91).

So that changes our API methods to

```haskell
class YesodNestedDispatch parent nest | nest -> parent where
    type ParentArgs nest :: Type
    type ParentArgs = ()
    yesodNestedDispatch 
        :: (Yesod parent) 
        => ParentArgs nest
        -> YesodRunnerEnv parent
        -> Request
        -- ^ original request:
        -> [Text]
        -- ^ remaining path fragments
        -> HandlerFor site TypedContent
```

It'd be interesting to see if `Yesod parent` is even necessary here.
Or `YesodRunnerEnv`.
Since we're just returning essentially `fmap toTypedContent methodRouteNameHereR`, and allowing the parent to actually *run* the `Handler`, we should be doing alright.

```haskell
class YesodNestedDispatch nest where
    type ParentArgs nest :: Type
    type ParentArgs = ()
    yesodNestedDispatch 
        :: ParentArgs nest
        -> Text
        -- ^ Method
        -> [Text]
        -- ^ remaining path fragments
        -> HandlerFor site TypedContent
```

So, the form of what we're going to do, is generate:

```haskell
instance YesodNestedDispatch ParentR where
    type ParentArgs ParentR = Int

    yesodNestedDispatch parentArg0 method fragments =
        helper fragments
      where
        toTypedContent' = fmap toTypedContent
        helper (fromPathPiece -> Just dyn0 : "child" : []) =
            case method of
                "GET" ->
                    toTypedContent' $ 
                        getChildR parentArg0 dyn0
                _ ->
                    toTypedContent' $
                        void badMethod
        helper _ =
            toTypedContent' $
                void notFound
```

Let's get this working, and then I'll do the TH.
