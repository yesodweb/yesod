# Prototype 3: leaf policies selected by dispatch

This prototype combines local `RouteLeaves` views with a dispatch wrapper. It
keeps authorization instances beside their owning dispatch modules and resolves
the required dictionaries when an application or fragment request is assembled.
The ordinary `Yesod` instance and test foundation provider do not require the
whole site's authorization graph.

## Why dispatch supplies the dictionary

Middleware can inspect `getCurrentRoute`, but that value does not contain a
user-chosen class dictionary. A sum-dictionary visitor such as
`withCurrentRouteLeaves @AuthorizeRoute` must therefore demand the policies for
all possible fragments. Storing that visitor on the foundation moves the same
requirement to foundation construction. Neither preserves isolated fragment
requests with the ordinary provider.

Dispatch already knows the selected fragment. The TH hook supplies its local
leaf value to a callback and puts the callback's class constraint on the
fragment's dispatch instance. This needs no existential value in the request,
foundation callback field, middleware registry, or change to hspec-yesod.

## Application API

```haskell
class HasRouteLeaves route => AuthorizeRoute route where
    authorizeRoute
        :: ParentArgs route
        -> RouteLeaves route
        -> HandlerFor (ParentSite route) ()

routeOpts = setRouteLeafHandlerWrapper [t| AuthorizeRoute |]
    (\handler args leaf -> [| authorizeRoute $args $leaf >> $handler |])
    defaultOpts
```

Use these same options for data and dispatch splices. Data generation creates
local views without evaluating the constraint quotation or wrapper callback.
`setRouteLeafViews True` also exposes the structural views independently.

For a fragment with direct endpoints and a nested child, the leaf view contains
only the direct endpoints:

```haskell
-- Routes generated from the route file:
-- data TeamR = SettingsR Int | MembersR MemberR
-- data instance RouteLeaves TeamR = LeafSettingsR Int

instance AuthorizeRoute TeamR where
    authorizeRoute parent (LeafSettingsR setting) = validate parent setting
```

There is no `MembersR` case to fill in. Matching a delegation constructor against
`RouteLeaves TeamR` fails to typecheck; `-Werror=incomplete-patterns` checks that
all direct endpoints are handled. This checks the shape and coverage of the
policy, not its security semantics. A child policy must include any required
ancestor checks; ancestor policies are not invoked on the way to a child.
`projectRouteLeaves`, `fromRouteLeaves`, and `fillInNested` provide pure adapters
for incremental migration of existing route functions.

### Migrating ancestor checks

`RouteLeaves` is local to one fragment. A request for a nested child invokes
only the selected child's leaf policy; it cannot also match an ancestor's
delegation constructor in an ancestor `RouteLeaves` instance. Ancestor
authorization instances are not an enforcement layer in this design.

Before migrating a nested endpoint, trace its entire legacy authorization path.
Carry every check on that path into the new leaf policy, including checks that
ran before or after delegation. Use `ParentArgs` to access ancestor captures,
and compose shared parent-check functions with the endpoint's own policy.
Preserve their ordering and failure behavior. For example, replacing
`validateTeam team >> authorizeMember member` with a member leaf policy must
retain `validateTeam team`; implementing only `authorizeMember member` loses
the team check.

Test a request whose leaf check passes but whose ancestor check fails, as well
as the successful request. Exhaustive leaf matching proves endpoint coverage;
it cannot prove that a migration preserved these authorization conditions.
Once migrated, replace the corresponding legacy case with an explicit wiring
error so accidental legacy calls fail visibly instead of using a stale policy.

## Dictionary boundaries

The generated code has the following shape (omitting unrelated constraints):

```haskell
instance (AuthorizeRoute TeamR, YesodDispatchNested MemberR)
      => YesodDispatchNested TeamR where ...

instance AuthorizeRoute MemberR => YesodDispatchNested MemberR where ...
```

A purely delegating fragment needs only its child dispatch constraints. A
fragment request needs its own subtree's policies; the complete application
needs the entire dispatch tree. Imported child dispatch instances retain their
own configuration. Rendering and redirects do not demand authorization.
Generated contexts may require `FlexibleContexts` and `UndecidableInstances`.

An application whose policy class depends on the foundation can use a small,
foundation-independent bridge class in its shared route options. The application
class can still return its existing policy type, with one shared bridge instance
interpreting values from that class. This breaks the route-data/options/policy
module cycle without separate data/dispatch options or widespread import edits.

For an incremental draft, an overlappable application policy instance can
delegate to the original policy selector after reconstructing the full route
with `toParentRoute`. Thread the `isWrite` argument supplied by
`dispatchAuthorizationCheck` into the policy method when legacy policies use it.
Import that fallback when assembling legacy requests or the complete application,
not in the shared provider, general test prelude, bridge class, or migrated
dispatch modules. It intentionally retains the legacy policy graph for those
callers; explicit local instances are the migration endpoint.

## Enforcement and compatibility

The hook runs inside normal Yesod middleware, immediately before the selected
handler, including matched-path 405s. Unmatched paths have no leaf and skip it.
A callback can call `dispatchAuthorizationCheck` to preserve Yesod's `AuthResult`
handling and write-request classification. An application migrating its existing
check to dispatch can set `Yesod.isAuthorized _ _ = pure Authorized` and retain
`defaultYesodMiddleware`.

Existing users who do not enable the option retain their dispatch behavior and
instance contexts. No fields are added to exported runtime records. The older
plain `setRouteHandlerWrapper` and named authorizer options remain available;
setting either wrapper replaces the other.

Subsite mounts require a named mount policy, including their unmatched paths.
Every subsite on that path must honor `ysreParentRunner`; TH cannot establish
that contract for arbitrary or transitively mounted implementations. A raw WAI
subsite which bypasses the parent runner also bypasses this policy, just as it
bypasses the existing parent middleware. See [the split-route guide](split-route-compilation.md)
for the mount contract and split-splice configuration.

## Validation

The normal yesod-core suite exercises root and fragment dispatch, parent
captures, multipieces, failures, 405s, 404s, middleware ordering, and error
handling. `bash yesod-core/test/check-route-leaf.sh` additionally builds and runs
an application with only its account policy in scope, then checks compiler
rejections for a missing policy, a delegation pattern, and an incomplete local
policy. This separate compilation prevents whole-suite imports from accidentally
satisfying the isolation test.
