# Prototype: authorization through route-leaf middleware

This branch prototypes the middleware alternative to dispatch authorization
hooks. TH generates structural views and dictionary selection; ordinary
`yesodMiddleware` selects and runs the application-owned policy. The generated
dispatcher receives no authorization callback.

The branch targets master directly and is independent of the dispatch-hook
proposal. It uses the existing default middleware unchanged.

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

its generated `RouteLeaves OrgR` contains only `LeafOrgHomeR`. It has no
delegation constructor. `RouteLeaves` describes this structural view independently
of the constraint used to visit it; authorization is one application. An
instance such as this is exhaustive:

```haskell
instance AuthorizeRoute OrgR where
    isAuthorized org LeafOrgHomeR = checkOrganizationHome org
```

This is local to one level: `RouteLeaves (Route Site)` contains only root
endpoints, and `RouteLeaves AccountR` contains only endpoints directly owned by
`AccountR`. Neither includes leaves belonging to nested children.

The application defines the class, including its result type:

```haskell
class HasRouteLeaves fragment => AuthorizeRoute fragment where
    isAuthorized
        :: ParentArgs fragment
        -> RouteLeaves fragment
        -> HandlerFor (ParentSite fragment) AuthResult
```

The library does not require `AuthResult`; an application can retain
`AuthPolicyStyleWrapper` and its existing interpreter. The instance stays beside
the fragment's dispatch instance.

For each fragment with direct endpoints, generation supplies:

* A `RouteLeaves` data instance, with `Leaf` prefixed to each local endpoint's
  constructor name and the original endpoint fields retained.
* `projectRouteLeaves`, a shallow conversion returning `Nothing` for delegation
  constructors, and `fromRouteLeaves`, its local-endpoint embedding.
* A witness such as `FragmentAccountR`. The root witness is `FragmentRouteSite`
  for site `Site`, when the root owns direct endpoints.

`RouteFragmentWitness wholeRoute fragment` identifies an endpoint-owning
fragment within the full route type. For example:

```haskell
FragmentAccountR :: RouteFragmentWitness (Route Site) AccountR
FragmentRouteSite :: RouteFragmentWitness (Route Site) (Route Site)
```

`wholeRoute` is the entire site's route sum; `fragment` is the selected
endpoint-owning sum, which may be the root itself. The witness identifies its
type; `RouteLeaves fragment` carries the selected endpoint and its local captures.

A full-site data splice also emits `RouteLeafSelection site` and the generic instance:

```haskell
instance (c (Route Site), c OrgR, c AccountR, ...)
    => RouteFragmentDict c (Route Site)
```

Only endpoint-owning fragments appear in that context. Pure grouping fragments
require no policy instance. Focused data splices generate local views; a later
full-site splice reuses those instances and constructs the site-wide table.
Neither step looks for authorization instances.

`getRouteFragmentDict` returns the upstream `Data.Constraint.Dict` from the
`constraints` package, re-exported by `Yesod.Core.RouteLeaf`.

`fillInNested authorize onNested` adapts a leaf-only function to the original
fragment type. The generated shallow projection fills every delegation branch
with `onNested`. A nested-route pattern in `authorize` is a type error. Missing
endpoint cases can be rejected with `-Werror=incomplete-patterns`; a wildcard
still opts out of that coverage check. A throwing `onNested` remains a runtime
assertion. The middleware visitor supplies the leaf view directly and needs no
such assertion.

## Middleware and dependency ownership

For a statically selected fragment, middleware can fetch its local leaf value:

```haskell
getDeepestLeaves
    :: (LookupRouteLeaves fragment, RouteLeafSelection (ParentSite fragment))
    => HandlerFor (ParentSite fragment) (Maybe (RouteLeaves fragment))

getDeepestLeaves @AccountR
    :: HandlerFor Site (Maybe (RouteLeaves AccountR))

getDeepestLeavesWithParentArgs @AccountR
    :: HandlerFor Site (Maybe (ParentArgs AccountR, RouteLeaves AccountR))
```

The full-site data splice generates `LookupRouteLeaves` instances for these
getters. Selection and lookup are pure; the handler wrapper only reads
`getCurrentRoute`. They need no authorization dictionaries. `Nothing` means either no
matched route or a matched endpoint owned by another fragment, including a
deeper child. Parent captures are kept separate from the local leaf; use the
second getter when the policy needs them. A focused middleware must reject
unexpected matched fragments rather than silently skip authorization.

For middleware that chooses the deepest fragment at runtime, retain the
dictionary visitor. It supplies that fragment's local `RouteLeaves` value to the
application-owned class method. The getter's type parameter is chosen by its
caller; a request cannot change that result type dynamically:

```haskell
authorizationMiddleware handler = defaultYesodMiddleware $ do
    checked <- getDeepestSubrouteWithInstance @AuthorizeRoute $ \args leaf ->
        enforceAuthorization =<< isAuthorized args leaf
    case checked of
        Nothing -> handler -- explicit policy for unmatched requests
        Just () -> handler

-- Application-owned response policy; choose redirects here if desired.
enforceAuthorization :: AuthResult -> HandlerFor site ()
enforceAuthorization Authorized = pure ()
enforceAuthorization AuthenticationRequired = notAuthenticated
enforceAuthorization (Unauthorized message) = permissionDenied message
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
    isAuthorized _ _ = pure Authorized
    yesodMiddleware handler = do
        app <- getYesod
        requestMiddleware app handler
```

The foundation imports no authorizers. The application construction module
imports them and supplies the middleware value. This changes the application's
own foundation constructor, not a Yesod runtime environment record.

The legacy `Yesod.isAuthorized` is a no-op because the supplied middleware
enforces the leaf policy. `defaultYesodMiddleware` still supplies the normal
headers and calls `isWriteRequest` for matched routes. The example's policy
interpreter returns 401 when authentication is required; applications can
supply their own login redirect or other response behavior.

A focused test must avoid constructing the full-site dictionary if it wants to
exclude sibling policies. It can use `getDeepestLeavesWithParentArgs @AccountR`
and call the same leaf instance as production. The prototype's
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

Once the leaf policies enforce the required checks, set the legacy
`Yesod.isAuthorized _ _ = pure Authorized` and retain `defaultYesodMiddleware`.
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
