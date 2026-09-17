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
them — along with your project's `RouteOpts`. Derive every splice's options
from this shared value so route types and fallthrough stay consistent (see
[Fallthrough](#fallthrough) for why fallthrough should be on). Focus and
authorization options may differ by splice; subsite dispatch must clear the
site-only authorization options described below:

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

## When the subsite split is required, not optional

Everything above presents the Data/Dispatch split as an organizational choice.
It stops being optional once a *parameterized* subsite mounts a *different*
subsite inside its own route tree — a subsite leaf nested under a
`ResourceParent`, not just a further nested route group of the same subsite.

`mkYesodSubData` generates the subsite's route datatype and its `resources*` value
(a `[ResourceTree String]`) in one splice. Turning that value into dispatch
code — `mkYesodSubDispatch`, `mkNestedSubDispatchInstance` — is a *second*
splice. GHC's stage restriction won't let a top-level splice consume a name
that a different splice produced earlier in the same module; that name has to
come from an already-compiled module instead. `mkYesodSubDispatchInstance`
gets around this by generating the `YesodSubDispatch` and nested instances in
one splice, which is why it can keep a parameterized subsite's own dispatch in
a single module —
but it only covers that one subsite's own nested route groups. A mounted child
subsite needs its own `mkYesodSubData` and its own `YesodSubDispatch`
instance, and the parent's `resources*` value then has to cross a module
boundary to reach the splice that dispatches into it.

`yesod-core`'s test suite has a worked example of exactly this shape:
`YesodCoreTest.ParameterizedSubDispatchRuntime.Data` generates a parameterized
subsite (`BigSub a`) that mounts a second, unparameterized subsite (`ChildSub`)
as a leaf, and `YesodCoreTest.ParameterizedSubDispatchRuntime` is the separate,
already-compiled module that dispatches both.

## Authorization in a fragment

`setRouteHandlerWrapper` gives your library an opt-in TH hook with the handler
expression first and a `WithParentArgs fragment` expression second:

```haskell
authRouteOpts :: RouteOpts
authRouteOpts = setRouteHandlerWrapper
    (\handler route -> [| requireAuthorized $route >> $handler |])
    appRouteOpts
```

`requireAuthorized` runs before the handler. It can call your own authorization
function, inspect an application-defined `AuthorizationResult a`, and throw
`permissionDenied` or `notAuthenticated` on failure. The hook does not require
Yesod's `AuthResult` or impose a type on the callback's successful result.
The handler expression and returned expression both have type
`HandlerFor site TypedContent`, including for method-mismatch handlers. The TH
callback runs once per generated leaf handler and is skipped by data-only
splices such as `mkYesodDataOpts`.

For class dispatch, your library can define its own `isAuthorized` method and
use it both from `requireAuthorized` and directly in other handlers:

```haskell
import Yesod.Core hiding (isAuthorized)

class RenderRouteNested route => AuthorizeRoute route where
    isAuthorized
        :: WithParentArgs route
        -> HandlerFor (ParentSite route) (AuthorizationResult ())
```

Here `AuthorizationResult` is your library's type. Put the `AuthorizeRoute`
instance for a fragment alongside its dispatch, and use `authRouteOpts` in its
focused splice. Generated dispatch passes that fragment's type to the hook,
so GHC resolves only that fragment's authorization instance. The foundation's
`instance Yesod App` needs no imports of those instances. A parent dispatch
that delegates to a separately compiled fragment uses the wrapper and named
authorization policy selected by the fragment's splice. Neither option is
inherited from the parent, and the parent cannot check which policy an opaque
existing instance used. Configure and test each fragment's dispatch explicitly.

Derive these options from the shared `appRouteOpts` so fallthrough and route
type settings stay consistent. If the shared options already include a
wrapper, `unsetRouteHandlerWrapper appRouteOpts` removes it for a splice that
needs different authorization without resetting the other options.

`WithParentArgs` contains all ancestor captures and the matched fragment,
including its leaf captures and trailing multipieces. For example,
`/org/#Int OrgR: /account/#Text AccountR: /item/#Int ItemR GET` supplies
`WithParentArgs (orgId, accountName) (ItemR itemId)` to `AccountR`'s wrapper.
Top-level leaves receive `WithParentArgs () fullRoute`; so do leaves inlined
for compatibility instead of using nested dispatch.

The wrapper runs inside the site's middleware, after any named authorization
check. It also wraps the 405 handler for a matched path with an unsupported
method, allowing authorization to fail before the 405 is reported. Unmatched
paths do not invoke it. The wrapper does not wrap subsite mounts or handlers
inside a mounted subsite. A splice with a wrapper and a mount must also enable
`setRouteAuthorization RouteAuthPerResource` or `RouteAuthSubtree` and provide
`authorize<MountName>` with the ancestor and mount captures. TH rejects
wrapper-only mounts. Enabling a named policy requires bindings for every
leaf emitted by that splice, even those already guarded by the wrapper.
To keep other leaves wrapper-only, place the mount alone under a parent route
and focus a named dispatch splice on that parent. A mount leaf cannot itself
be a focus target; focusing an existing parent also emits its other leaves,
which would need named bindings. The named mount policy runs through the parent
runner, including on a subsite 404, where there is no subsite route value to
supply to the wrapper.

Subsite dispatch splices reject both authorization options. Derive their
options from the shared value:

```haskell
appSubsiteOpts :: RouteOpts
appSubsiteOpts = subsiteRouteOpts authRouteOpts
```

`WaiSubsite` and `EmbeddedStatic` bypass the parent runner. Dispatch generation
rejects direct named mounts of these types, along with unresolved type names
and type variables or type families. Import the concrete subsite type in the
dispatch module; ordinary type synonyms are supported. Use `WaiSubsiteWithAuth` to apply the
parent's middleware and authorization to a WAI application.

This runner requirement applies at every level: mounting a `WaiSubsite` or
`EmbeddedStatic` inside a generated subsite still bypasses the outer named
mount authorizer. Replace raw WAI subsites with `WaiSubsiteWithAuth` throughout
the dispatch path. TH cannot inspect transitive mounts or arbitrary instance
bodies; a generated outer subsite alone does not guarantee protection.

When a subsite route matches, the named check calls the site's `isWriteRequest`
override. On a subsite 404, there is no route to pass to that method, so the
default method policy applies: GET, HEAD, OPTIONS, and TRACE are reads; other
methods are writes. The legacy `isAuthorized` check skips these misses while
the named mount policy still runs. An `AuthenticationRequired` result can
replace the 404 with a login redirect or 401, without changing `_ULT` when no
route matches. For a method policy shared by hits and misses, the mount
authorizer can inspect `waiRequest` directly.

The existing `Yesod.isAuthorized` still runs through `defaultYesodMiddleware`.
Leave its default implementation when moving authorization into fragments.
Calling your library's `isAuthorized` directly checks another fragment without
requiring a site-wide authorizer; Yesod's `maybeAuthorized` continues to use
the legacy `Yesod.isAuthorized` method.

Without `setRouteHandlerWrapper`, generation and authorization behave as before.
The hook also composes with the named `setRouteAuthorization` policies. See
the `RouteAuthSpec` Haddock for the ordering contract: default middleware's
site-wide check, named check, wrapper, and handler body.

`RouteAuthSubtree` demands `authorize<SubtreeName> parentCaptures fragment` for
the nearest enclosing parent of each method-based leaf, in both nested and
inline compatibility dispatch. Ancestor policies do not compose automatically.
For `/org/#Int OrgR: /account/#Text AccountR: /item/#Int ItemR GET`, only
`authorizeAccountR org account (ItemR item)` runs; put any organization access
check there or call a shared helper from it. A parent containing only other
parents or mounts needs no subtree binding. Top-level leaves and subsite
mounts use `authorize<ResourceName> captures`; mount authorizers receive the
ancestor and mount captures, not a child subsite route.

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
`appRouteOpts`/`appRouteOptsFor` once and derives each splice's options from
them. Keep route-shape and fallthrough settings shared; change focus and
authorization at the owning splice as described above, and clear the site-only
authorization options for subsite dispatch.

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
* **`GHC stage restriction: 'resourcesFoo' is used in a top-level splice,
  quasi-quote, or annotation, and must be imported, not defined locally`** —
  a `mkYesodSubData`-generated `resources*` value is being consumed by another
  splice (`mkYesodSubDispatch`, `mkNestedSubDispatchInstance`) in the same
  module. Move the consuming splice to a separate, already-compiled module
  that imports the `resources*` binding (see [When the subsite split is
  required, not optional](#when-the-subsite-split-is-required-not-optional)),
  or, if the subsite mounts no child subsite, replace both splices with a
  single `mkYesodSubDispatchInstance` call.
