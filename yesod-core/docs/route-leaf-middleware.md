# Prototype: authorization through route-leaf middleware

This branch prototypes the middleware alternative to dispatch authorization
hooks. TH generates structural views and dictionary selection; ordinary
`yesodMiddleware` selects and runs the application-owned policy. The generated
dispatcher receives no authorization callback.

The branch targets master directly and is independent of the dispatch-hook
proposal. It includes the middleware helpers used below.

Enable the views through shared data/dispatch options:

```haskell
leafOpts = setRouteLeafViews True defaultOpts
```

Import `Yesod.Core.RouteLeaf` for the runtime API. Data splice modules need
`ConstraintKinds`, `GADTs`, `FlexibleContexts`, `FlexibleInstances`,
`MultiParamTypeClasses`, `TypeFamilies`, and `UndecidableInstances`, in addition
to the extensions needed by their existing routing declarations. Parameterized
nested routes must enable `setParameterizedSubroute True` too.

## What is generated

Given a mixed fragment:

```haskell
data OrgR = OrgHomeR | DelegationR DelegationR
```

its generated `AuthDispatch OrgR` contains only `AuthOrgHomeR`. It has no
delegation constructor. An instance such as this is exhaustive:

```haskell
instance AuthorizeRoute OrgR where
    isAuthorized org AuthOrgHomeR = checkOrganizationHome org
```

The application defines the class, including its result type:

```haskell
class HasAuthDispatch route => AuthorizeRoute route where
    isAuthorized
        :: ParentArgs route
        -> AuthDispatch route
        -> HandlerFor (ParentSite route) AuthResult
```

The library does not require `AuthResult`; an application can retain
`AuthPolicyStyleWrapper` and its existing interpreter. The instance stays beside
the fragment's dispatch instance.

For each fragment with direct endpoints, generation supplies:

* An `AuthDispatch` data instance, with `Auth` prefixed to each local endpoint's
  constructor name and the original endpoint fields retained.
* `projectAuthDispatch`, a shallow conversion returning `Nothing` for delegation
  constructors, and `fromAuthDispatch`, its local-endpoint embedding.
* A witness such as `LeafAccountR`. The root witness is `LeafRouteSite` for site
  `Site`, when the root owns direct endpoints.

A full-site data splice also emits `RouteLeaves site` and the generic instance:

```haskell
instance (c (Route Site), c OrgR, c AccountR, ...)
    => SubrouteDict c (Route Site)
```

Only endpoint-owning fragments appear in that context. Pure grouping fragments
require no policy instance. Focused data splices generate local views; a later
full-site splice reuses those instances and constructs the site-wide table.
Neither step looks for authorization instances.

`getSubrouteDict` returns the upstream `Data.Constraint.Dict` from the
`constraints` package, re-exported by `Yesod.Core.RouteLeaf`.

`fillInNested authorize onNested` adapts a leaf-only function to the original
fragment type. The generated shallow projection fills every delegation branch
with `onNested`. A nested-route pattern in `authorize` is a type error. Missing
endpoint cases can be rejected with `-Werror=incomplete-patterns`; a wildcard
still opts out of that coverage check. A throwing `onNested` remains a runtime
assertion. The middleware visitor supplies the leaf view directly and needs no
such assertion.

## Middleware and dependency ownership

```haskell
authorizationMiddleware handler = defaultYesodMiddlewareNoAuthCheck $ do
    checked <- getDeepestSubrouteWithInstance @AuthorizeRoute $ \args leaf ->
        dispatchAuthorizationCheck (const $ isAuthorized args leaf)
    case checked of
        Nothing -> handler -- explicit policy for unmatched requests
        Just () -> handler
```

The callback receives all ancestor captures and the local endpoint. It runs
once; it neither invokes ancestor authorizers nor falls back after a denial.
The pure `withRouteLeaf @c` visitor supports other constraints on the same
generated table. `Nothing` from the handler visitor means no current route,
not a missing dictionary. Dictionary coverage is checked at compile time.

Using the full-site dictionary requires every policy listed in its context.
Assemble this middleware in the application construction module, then inject it
through a foundation field:

```haskell
data App = App
    { requestMiddleware :: forall a. HandlerFor App a -> HandlerFor App a
    -- other fields
    }

instance Yesod App where
    yesodMiddleware handler = do
        app <- getYesod
        requestMiddleware app handler
```

The foundation imports no authorizers. The application construction module
imports them and supplies the middleware value. This changes the application's
own foundation constructor, not a Yesod runtime environment record.

A focused test must avoid constructing the full-site dictionary if it wants to
exclude sibling policies. It can use `routeLeaf`, match its known witness, and
call the same leaf instance as production. The prototype's
[account module](../test/YesodCoreTest/RouteLeaf/Account.hs) demonstrates this
without importing root dispatch or sibling authorizers. Unexpected witnesses
are rejected, and an independent executable verifies that dependency boundary.
No hspec-yesod library changes are needed for this wiring.

## Migration and limits

Parent authorization that previously validated captures before delegating must
move those required checks into the leaf policy or helpers it explicitly calls.
The compiler excludes delegation branches from the new input type; tests still
need to establish that the migration preserved the required checks. The account
fixture denies requests with invalid organization, account, or endpoint captures.

Use `defaultYesodMiddlewareNoAuthCheck` when composing this middleware so that
the legacy `isAuthorized` policy is replaced rather than run a second time.
Existing users retain their current output and behavior when the new option is
disabled. The leaf-view API can also be consumed by a future class-based
dispatch implementation.

Mount views retain their own captures and the child route, stopping at the site
boundary. A compliant subsite's matched request can be authorized in parent
middleware. A subsite miss can have no parent route, and raw WAI applications
that bypass the parent runner bypass its middleware. This prototype retains
those limitations. Unmatched-route behavior is an explicit middleware decision.

This is an opt-in prototype, not a finalized public API. Generated constructor
naming and compile-time cost still need evaluation. An empty route tree is
rejected. Nested compatibility mode without parameterized subroutes is rejected
with an instruction to enable them. GHC 9.8.4 is the tested compiler; the broader
supported compiler matrix has not been run.

## Verification

```sh
stack test yesod-core --fast --no-terminal
yesod-core/test/check-route-leaf.sh
```

The regular suite covers ordinary/focused dispatch, mixed and pure delegation
fragments, default headers, 404/405, method classification once, policy/handler
order, parent/leaf captures, multipieces, unit arguments, mounted routes,
parameterized captures, and focused data splices followed by full-site data
generation. The separate script builds an isolated account application and
requires compiler rejection of a nested pattern, an omitted endpoint, missing
policy dictionaries, and unsupported parameterized compatibility mode.
