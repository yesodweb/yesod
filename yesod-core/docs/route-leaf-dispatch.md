# Application-owned policies selected by dispatch

`setRouteDispatchWrapper` lets applications wrap matched handlers while keeping
the required class dictionaries on generated dispatch instances. A fragment
request needs the dictionaries for its own dispatch subtree. Neither the site's
`Yesod` instance nor a test foundation provider needs to assemble all policies.

```haskell
class AuthorizeRoute route where
    authorizeRoute :: ParentArgs route -> route -> HandlerFor (ParentSite route) ()

routeOpts = setRouteDispatchWrapper [t| AuthorizeRoute |]
    (\handler route -> [|
        let WithParentArgs args fragment = $route
        in authorizeRoute args fragment >> $handler
    |]) defaultOpts
```

Use the same options for route data and dispatch. The callback and constraint
quotation are evaluated only by dispatch splices. Data generation stays free
of authorization dependencies. An application may set
`Yesod.isAuthorized _ _ = pure Authorized` when this wrapper replaces the old
check, retaining its existing middleware.

## Dictionary boundaries

Generated dispatch retains the supplied constraint for fragments with their
own resources, and asks for child dispatch dictionaries at nested delegation
boundaries. A purely delegating fragment needs no policy of its own. Imported
child dispatch instances keep their own configuration; an opaque handwritten
instance cannot be certified by the parent splice.

The wrapper runs inside `yesodMiddleware`, before the handler, including
matched-path 405s. Unmatched paths do not run it. `dispatchAuthorizationCheck`
can preserve Yesod's `AuthResult` and `isWriteRequest` semantics. Applications
can use a lightweight bridge class in their shared options to keep policy
implementations out of their route data modules.

## Subsites

At a mount, child dispatch selects the route before calling the parent runner.
The wrapper receives the owning fragment containing that actual child route,
with ancestor and mount captures intact. It does not run on a subsite miss.
`FromParentRoute` structurally recovers the fragment; it does not reparse the
request path. The root data splice generates these instances for fragments
with mounts when the constrained wrapper is enabled.

Applications can delegate authorization through the child route, requiring a
child-specific dictionary just as they do for a nested route fragment. Yesod
does not prescribe a leaf type or decide which constructors may carry policy.
An inconsistent route returned by a custom subsite is an internal error.

Mount wrappers share the existing middleware and session. Every subsite on the
path must honor `ysreParentRunner`. TH rejects direct mounts of known bypassing
types, but cannot inspect arbitrary or transitive implementations. A raw WAI
subsite bypasses this wrapper just as it bypasses the parent's middleware.
See [the split-route guide](split-route-compilation.md) for the runner contract.

The older named mount policies remain independent: they run before this wrapper
and still run on misses. The plain `setRouteHandlerWrapper` retains its earlier
endpoint-only behavior. Selecting either wrapper replaces the other.

## Application-owned route views

`setRouteDataGenerator` adds declarations to a data splice. Its callback receives
the context, type arguments, site type, optional focus name, and resolved route
tree. Dispatch splices do not invoke it. `parseResourceTypes` converts published
string-based subsite resources for an application generating views separately.

For example, an application can generate an endpoint-only `RouteLeaves route`
sum, omitting both nested fragments and subsite mounts. A separate generated
visitor can require a policy for local endpoints and child visitor dictionaries
for delegation constructors. Such a visitor belongs to the application: its
representation, recursion, and migration rules can evolve without changing
Yesod. The same options can combine that data generator with a dispatch wrapper.

This checks dictionary availability and constructor coverage, not policy
semantics. Migrations must explicitly preserve every legacy ancestor check,
its ordering, and its failure behavior.

## Compatibility and validation

Existing users who do not enable these options retain their dispatch behavior
and instance contexts. No exported runtime record gains a field. Ordinary
hspec-yesod requests exercise the generated wrapper without a new request API.

The normal suite covers root and focused dispatch, separate policy modules,
parameterized sites, captures, multipieces, 405s, misses, session sharing, and
error handling. `bash yesod-core/test/check-route-leaf.sh` separately compiles
and runs an isolated fragment, then verifies that a missing policy fails to
compile. Application-specific leaf coverage and delegation tests live downstream.
