# `setUrl` Call Stack

`setUrl :: (Yesod site, RedirectUrl site url) => url -> RequestBuilder site)`.

So `RedirectUrl` is how we go from an endpoint to an actual url:

```haskell
    eurl <- Yesod.Core.Unsafe.runFakeHandler
        M.empty
        (const $ error "Yesod.Test: No logger available")
        site
        (toTextUrl url')
```

is the exact code.

```haskell
-- | Some value which can be turned into a URL for redirects.
class RedirectUrl master a where
    -- | Converts the value to the URL and a list of query-string parameters.
    toTextUrl :: (MonadHandler m, HandlerSite m ~ master) => a -> m Text

-- the instance for routes:

instance RedirectUrl master (Route master) where
    toTextUrl url = do
        r <- getUrlRender
        return $ r url

-- | Get the URL rendering function.
getUrlRender :: MonadHandler m => m (Route (HandlerSite m) -> Text)
getUrlRender = do
    x <- rheRender <$> askHandlerEnv
    return $ flip x []

-- where that is, anyway
data RunHandlerEnv child site = RunHandlerEnv
    { rheRender   :: !(Route site -> [(Text, Text)] -> Text)
    , rheRoute    :: !(Maybe (Route child))
    -- snip
    }

```

`rheRender` appears to be populated in two places: one in `yesodRunner` and one in `fakeRunHandler`.

```haskell
yesodRunner = ...
      let ra = resolveApproot yreSite req
      let log' = messageLoggerSource yreSite yreLogger
          -- We set up two environments: the first one has a "safe" error handler
          -- which will never throw an exception. The second one uses the
          -- user-provided errorHandler function. If that errorHandler function
          -- errors out, it will use the safeEh below to recover.
          rheSafe = RunHandlerEnv
              { rheRender = yesodRender yreSite ra
              -- snip
              }

runFakeHandler = ...
  let handler' = liftIO . I.writeIORef ret . Right =<< handler
  let yapp = runHandler
         RunHandlerEnv
            { rheRender = yesodRender site $ resolveApproot site fakeWaiRequest
            , rheRoute = Nothing
            -- snip
            }
```

So both call to `yesodRender`.

```haskell
yesodRender :: Yesod y
            => y
            -> ResolvedApproot
            -> Route y
            -> [(Text, Text)] -- ^ url query string
            -> Text
yesodRender y ar url params =
    decodeUtf8With lenientDecode $ BL.toStrict $ toLazyByteString $
    fromMaybe
        (joinPath y ar ps
          $ params ++ params')
        (urlParamRenderOverride y url params)
  where
    (ps, params') = renderRoute url
```

Ah, which uses `renderRoute` !

Well, this is... inconvenient.
`RenderRoute` will also need to be refactored in order to render the whole path, not just the fragment that it has.
So `RenderRoute` needs to know about the parent args.
This is somewhat inconvenient.

```haskell
class RenderRouteNested a where
    type ParentArgs a

    renderRouteNested :: ParentArgs a -> a -> ([Text], [(Text, Text)])
```

But it does.. actually somewhat simplify the logic for writing that instance.
If we get to a `ResourceParent`, we just `renderRouteNested`, pass in the accumulated parent args, and move on.

Does this call for a slight redesign on `parseRouteNested` also?
I think probably so.
But since that's not, actually, uh, used for anything, I am going to punt on it.

Anyway.

## Refactoring RenderRouteNested

OK, let's refactor RenderRouteNested to produce an entire rendered route for a fragment.
This means that our render function will need to be aware of the static path pieces as well as the dynamic path pieces, and then stitch them in together.
So at the top-level, our `ResourceLeaf` constructors will just "do the thing."
A `ResourceParent` will *immediately delegate* to `renderRouteNested`, passing in the dynamic pieces as the `ParentArgs` tuple.

While deriving the `RenderRouteNested`, when traversing the `[ResourceTree a]`, we will accumulate the prefix pieces.
This will be used to understand the dynamics when creating the `ParentArgs` as well as know the string static pieces to use.
