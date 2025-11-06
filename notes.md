# Splitting Yesod Routes Part Two

## Allowing Fallthrough in Nested Route Generation

Let's consider some examples.

```haskell
instance YesodDispatch App where
    yesodDispatch req = helper (pathPieces req)
      where
        method = ...
        -- Top-level route, leaf.
        helper [] =
            runHandler handleIndexR IndexR
        -- Top-level route, branch, delegate
        helper ("foo" : rest) 
            | Just (handler, constr) <- yesodDispatchNested () rest method
            = runHandler handler (FooR <$> constr)
        -- Top-level route, branch, inline. If I want fallthrough, I need to
        -- make this pattern match fail, which means I need some function which I
        -- can match on... so I probably should just push the 
        helper ("bar" : rest) = barHelper rest
            where
                barHelper [] = 
                    case method of
                        "POST" -> runHandler postBarHelper (Just (BarR Index'R))
                        _ -> runHandler badMethod Nothing
                barHelper 
```

So, in the `YesodDispatch` instance, we are matching on `Just (handler, constr)` as the result of `yesodDispatchNested`.
But we don't need to modify the leaves - these should still be `runHandler`.

Right now, we're generating these functions inline.
This makes fallthrough a big refactor differenfe

One edge case is that - if our route matches - then we 405 if the verb doesn't match.
This prevents us from providing a 

Now, considering `YesodDispatchNested`.

```haskell
instance YesodDispatchNested FooR where
    type Site FooR = App
    type ParentArgs = ()
    yesodDispatchNested () rest method = helper rest
      where
        helper [] = 
            case method of
                "POST" -> post
```
