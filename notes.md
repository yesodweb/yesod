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
                "POST" -> postHelper
                _ -> status405
        help
```

# IDEA: Just rewrite the TH for dispatch and parsing

The whole parsing/dispatch code is a massive mess.
Honestly I think I can rewrite it and be better off.

# ANYWAY

Let's make a table of possibilities...

* Parent class: `YesodDispatch` or `ParseRoute`
* Nesting class: Above + `Nested` (1:1)
* Generation: I can either be generating with a top-level (and thus will be delegating to nested classes + generating nested classes) or generating with a nested class (and thus will be delegating in the same way)

So, that means that, we really have a *special case* - top-level code is going to need to produce the "nested dispatch -> top-level dispatch". But everything else should be the same.

# Parsing Routes is Separate!

OK, thankfully, I have rewritten the route parsing code.
That was very straightforward and easy to do.
Now I can untangle the dispatch code so it isn't so much of a bear to work with.

# The Desolation of mkMDS

OK, so `mkMDS` is used to customize the `MkDispatchSettings`.
It accepts three arguments:

* `mdsUnwrapper = f`
* `mdsRunHandler = rh`
* `mdsSubDispatch = sd`

Ah, damn, `MkDispatchSettings` is actually entirely exported.
Damn, damn, damn.
So, this is going to be a breaking change if I remove or touch *any* of it - but it already is, because I have added record fields to it!

Alas.

OK, what's the game plan?
I think I will go ahead and make it much less terrible and accept that this huge functionality bump is going to be a major bump breaking change, but for a vanishingly small set of people using super weird and advanced functionality to custom hand-craft their instances.
Whatever.

So part of the difficulty here is that the actual code is different with `yesodDispatch` and `yesodDispatchNested` - like the APIs are pretty different.
With `ParseRoute{,Nested}`, it was easy, because the functions had the same types.
So generating code was really really simple.

But!! Another difficulty is that we need to retain the ability to dispatch on the *entire* route.
So probably `YesodDispatchNested` should accept the `WAI.Request` directly instead of letting the parent dig out the handler.
This simplifies the API because now the input type is the same... sort of.

# Working Backwards (Again?)
