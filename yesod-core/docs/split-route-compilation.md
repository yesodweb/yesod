# Split route compilation

As of yesod-core 1.7, route definitions can be split across multiple modules.
A nested route block gets its own datatype, instances, and dispatch generated
in its own module, and the main `mkYesod` splice *delegates* to that module
instead of regenerating everything in one place.

Why you'd want this:

* **Smaller TH splices.** A large site no longer needs one giant `mkYesod`
  splice that regenerates everything whenever any route changes.
* **Parallel compilation.** Each fragment module compiles independently.
* **Locality.** A route group's handlers, datatype, and dispatch live in one
  module, next to each other.
* **Faster test feedback.** Each fragment gets its own `YesodDispatchNested`
  instance, which is enough to dispatch a request on its own.
  [hspec-yesod](https://github.com/parsonsmatt/hspec-yesod) is building on
  this to run specs against a single route fragment, so a spec depends only on
  the handlers it actually exercises — editing an unrelated handler no longer
  recompiles (or relinks) the test module.

## How it works

Any nested route block — a parent declared with a trailing `:` — gets its own
route datatype:

```
/nest NestR:
    /     NestIndexR GET POST
    /#Int NestShowR  GET POST
```

By default, `mkYesod` generates the `NestR` datatype and its dispatch inline,
exactly as in 1.6. But if a `YesodDispatchNested NestR` instance is already in
scope at the splice site (because a separately compiled module generated it),
`mkYesod` delegates to that instance instead. Single-module sites are
unchanged; splitting is opt-in and per-parent.

## Recipe: top-level site

Three modules: a shared route table, the split-out fragment, and the main site.

First, put the route definitions in their own module so both sides can see
them — along with your project's `RouteOpts`. Every splice that touches the
route table should use the same options, so define them once (see
[Fallthrough](#fallthrough) for why fallthrough should be on):

```haskell
module App.Routes.Resources where

import Yesod.Core

appRouteOpts :: RouteOpts
appRouteOpts = setNestedRouteFallthrough True defaultOpts

appRouteOptsFor :: String -> RouteOpts
appRouteOptsFor name = setFocusOnNestedRoute name appRouteOpts

appResources :: [ResourceTree String]
appResources = [parseRoutes|
    /  HomeR GET

    /nest NestR:
        /     NestIndexR GET POST
        /#Int NestShowR  GET POST
|]
```

Then generate the `NestR` fragment in its own module by focusing the splice
on it. The fragment's handlers live here too:

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module App.Routes.NestR where

import Yesod.Core
import Data.Text (Text)
import App.Routes.Resources

mkYesodOpts (appRouteOptsFor "NestR") "App" appResources

getNestIndexR :: HandlerFor App Text
getNestIndexR = pure "nest index"

postNestIndexR :: HandlerFor App Text
postNestIndexR = pure "posted"

getNestShowR :: Int -> HandlerFor App Text
getNestShowR _ = pure "nest show"

postNestShowR :: Int -> HandlerFor App Text
postNestShowR _ = pure "posted"
```

Finally, the main module imports the fragment and splices with the same
options:

```haskell
module App where

import Yesod.Core
import App.Routes.Resources
import App.Routes.NestR (NestR (..))

mkYesodOpts appRouteOpts "App" appResources

getHomeR :: HandlerFor App Text
getHomeR = pure "home"
```

Because `App.Routes.NestR` compiled first, the `mkYesodOpts` splice sees the
`YesodDispatchNested NestR` instance and delegates to it rather than
regenerating the `NestR` code. If you want to separate the datatype from the
dispatch further, `mkYesodDataOpts` and `mkYesodDispatchOpts` accept the same
options.

## Recipe: subsites

Subsites split the same way. The data module generates the subsite's route
datatype and nested-fragment instances:

```haskell
module App.SplitSub.Data where

import Yesod.Core

data SplitSub = SplitSub

mkYesodSubData "SplitSub" [parseRoutes|
/ SplitHomeR GET
/nested NestedR:
    / NestedHomeR GET
    /detail/#Int NestedDetailR GET
|]
```

The split-out module holds the nested handlers and generates the
`YesodSubDispatchNested` instance with `mkNestedSubDispatchInstance`:

```haskell
{-# OPTIONS_GHC -Wno-orphans #-}

module App.SplitSub.NestedR (NestedR (..)) where

import Yesod.Core
import Yesod.Core.Dispatch
    (mkNestedSubDispatchInstance, defaultOpts, TyArgs (..))
import App.SplitSub.Data

getNestedHomeR :: SubHandlerFor SplitSub master Text
getNestedHomeR = pure "nested home"

getNestedDetailR :: Int -> SubHandlerFor SplitSub master Text
getNestedDetailR _ = pure "nested detail"

$(mkNestedSubDispatchInstance
    defaultOpts  -- or your project-wide RouteOpts; see Fallthrough below
    "NestedR"
    []        -- no instance context
    NoTyArgs  -- no type arguments (non-parameterized subsite)
    return    -- handler unwrapper
    resourcesSplitSub)  -- the [ResourceTree String] from your routes file
```

`mkNestedSubDispatchInstance` takes the resources as `[ResourceTree String]` —
exactly what the `parseRoutes` quasi-quoter produces — and parses the type
strings internally, failing the splice with an attributable error on a
malformed type rather than a deferred runtime `error`.

And the subsite's `YesodSubDispatch` instance delegates automatically, as long
as the split-out instance is in scope:

```haskell
import App.SplitSub.Data
import App.SplitSub.NestedR ()  -- instance import only; no handlers leak

instance YesodSubDispatch SplitSub master where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesSplitSub)
```

For a subsite defined entirely in one module (including parameterized subsites
like `data MySub a`), `mkYesodSubDispatchInstance "(MyClass a) => MySub a"
resourcesMySub` generates the `YesodSubDispatch` and nested instances in one
splice.

## Linking to nested routes

A nested fragment constructor isn't a `Route App` on its own — its parent may
bind dynamic pieces the fragment doesn't carry. To use a fragment in
`redirect` or `setUrl`, wrap it in `WithParentArgs` together with the parent's
dynamic arguments:

```haskell
redirect (WithParentArgs userId (ProfileEditR "name"))
```

When the parent binds no dynamic pieces, the bare constructor works directly —
generated `RedirectUrl`/`UrlToDispatch` instances fill in the empty parent
arguments:

```haskell
redirect NestIndexR
```

For this reason, if you are splitting up your routes for compilation, it is
recommended to avoid captures in the parent prefix (prefer
`/admin AdminR:` over `/user/#UserId UserR:`). With a static-only prefix,
every fragment constructor is usable directly in `redirect`/`setUrl`, and
`WithParentArgs` never enters the picture. Dynamic pieces can still live on
the individual child routes.

To convert a fragment into its parent's route type explicitly, use
`toParentRoute` from `Yesod.Core.Class.Dispatch.ToParentRoute` (not
re-exported from `Yesod.Core`, since the name is easy to collide with).

## Fallthrough

By default, once dispatch enters a nested parent whose subtree has no matching
child, the response is a 404 — later sibling routes are never tried. This
matters a lot when splitting: the usual first step is wrapping a batch of
previously-flat routes in a new nested group, and with fallthrough off, any
request matching the group's prefix now *commits* to that group. A route
declared later that shares the prefix silently becomes unreachable — flat
dispatch would have kept trying, the grouped version 404s. Enabling
`setNestedRouteFallthrough` restores the flat-dispatch behavior: a parent
whose subtree has no match falls through to the routes after it.

Fallthrough is decided per splice: each module containing a parent route
decides for its own parents. Mixing modules spliced with different options
gives confusingly inconsistent dispatch, which is why the recipe above defines
`appRouteOpts`/`appRouteOptsFor` once and uses them everywhere — don't reach
for `defaultOpts` directly in individual modules.

Related gotcha: a nested parent with *no* leading static path piece matches
unconditionally, so siblings declared after it are unreachable unless
fallthrough is enabled. Give the parent a static segment or enable
fallthrough.

## Troubleshooting

* **`Target 'NestR' was not found in resources.`** — the name passed to
  `setFocusOnNestedRoute` doesn't match any nested parent in the route table.
* **Missing-extension errors** — fragment-generating modules need
  `MultiParamTypeClasses`, and usually `FlexibleContexts` and
  `FlexibleInstances`. GHC's error names the missing extension.
* **Duplicate `RedirectUrl` instance** — the generated convenience instance is
  `OVERLAPPABLE`, so a *more specific* hand-written instance wins, but a
  hand-written instance with the identical head is still a duplicate-instance
  error.
