# TODO — `mattp/nested-route-discovery` review findings

> **Status: all 21 items resolved.** Library + both test suites green
> (`tests` 337/0, `test-routes` 77/0).

Review of the branch vs `master` (correctness · backwards-compat · test-coverage).
**Headline:** no runtime dispatch regression — the inline and `genNestedDispatchClauses`
paths are observably equivalent (verified by running the nested suite, 23/0). The work
below is API surface, functional asymmetries, test gaps, and cleanup.

---

## Backwards compatibility

- [x] **New instance surface for monomorphic sites + `RedirectUrl` overlap.**
  `discoveryMode` returns `NestedDiscovery` for any monomorphic site (`not hasArgs` is the
  first, unconditional guard — `RenderRoute.hs:270-275`), so every nested parent gets
  `YesodDispatchNested`/`RenderRouteNested`/`ParseRouteNested`/`ToParentRoute`, plus
  `UrlToDispatch Child Site` + `RedirectUrl Site Child` for no-dynamic-prefix parents
  (`Dispatch.hs:596-600,662-706`). No overlap pragma (`Internal.hs:23`). A user app with a
  hand-written `instance RedirectUrl App SomeChildR` now fails with duplicate instances, no
  opt-out. → Add a ChangeLog hazard note; consider an opt-out or `{-# OVERLAPPABLE #-}` on
  the generated `RedirectUrl`. (blast radius narrow: only no-prefix parents; `redirect SomeR`
  via `RedirectUrl master (Route master)` is unaffected.)

- [x] **`Yesod.Core` re-exports method names unqualified** (`Core.hs:13-22`) — injects
  `toParentRoute`, `parentArgsFor`, `ParentArgs`, `ParentSite` into every `import Yesod`.
  `toParentRoute` is a common local binding (already shadow-warns in yesod-auth
  `Auth/Email.hs:533`). → Export the classes/types without the method names unqualified.

- [x] **`RouteOpts(MkRouteOpts)` constructor now exported** (`RenderRoute.hs:15`); master
  kept it abstract (`defaultOpts` + setters). Defeats the compat boundary that made field
  additions safe. → Drop the constructor export; use a local helper for the record-match in
  `Dispatch.hs`.

- [x] **Document the full breaking TH-API set in the ChangeLog** (beyond `TyArgs`/`DiscoveryMode`):
  `mkDispatchClause` (new `DispatchPhase`/`[Name]`/`[Exp]` params + `Q ([String],Clause)`),
  `MkDispatchSettings` (4 new required fields), `mkParseRouteInstance` (`Q Dec`→`Q [Dec]`),
  `mkRouteConsOpts` (pure→`Q`), `instanceD` removed from `Yesod.Core.Internal.TH` exports.

- [x] **`findOverlapNames` narrowed** `[ResourceTree t]`→`[ResourceTree String]`
  (`Overlap.hs:66`). Unannounced public-signature narrowing. → Note in ChangeLog (or re-widen).

---

## Correctness / functional gaps

- [x] **Subsite nested-route fallthrough is unreachable via the public API.**
  `mkYesodSubDispatchInstance` takes no `RouteOpts` and hardcodes
  `setParameterizedSubroute True defaultOpts` (`Internal/TH.hs:270-273,352-354`,
  `roNestedRouteFallthrough = False`). Top-level threads it (`Dispatch.hs:516`); subsites
  can't. → Add a `RouteOpts` param or `mkYesodSubDispatchInstanceOpts`.

- [x] **`RouteAttrsNested` not generated for monomorphic single-module sites.**
  `Internal/TH.hs:215-220` uses flat `mkRouteAttrsInstance` when `roFocusOnNestedRoute =
  Nothing`; `mkRouteAttrsInstanceFor` (only `RouteAttrsNested` generator, `RouteAttrs.hs:31-38`)
  runs only on the focus path. So `routeAttrsNested ChildR` fails to resolve for a plain
  `mkYesod` while `renderRouteNested`/`parseRouteNested`/`yesodDispatchNested` resolve
  (proven via `FallthroughDispatch.dyn.dump-splices`). → Generate it alongside the others, or
  document the asymmetry.

- [x] **`checkNestedSubArity` only guards top-level nested datatypes.** Invoked once over the
  non-recursive `findNested` names (`Internal/TH.hs:312-316,345`); descendants from
  `mkNestedDispatchInstanceWith` recursion (`Dispatch.hs:641-648`) skip it, so a 2nd-level+
  parameterized subroute with wrong arity gets the cryptic kind error the check exists to
  prevent. → Run the check in the recursion. (Also add a deep-arity-mismatch test.)

- [x] **`shouldBeTH` is broken + dead** (`test/YesodCoreTest/Assertions/TH.hs:9-12`): body
  references unbound `a`/`b` instead of params `lhs`/`rhs`; module not in cabal, imported by
  nothing. → Fix or delete.

- [x] **(pre-existing, not a regression) zero-piece nested parent shadows siblings.**
  A parent with no leading piece → `mkPathPat EndWild [] == WildP` (`Internal.hs:289,294`),
  an unconditional first `Match` that makes later siblings unreachable under default
  `fallthrough=False` (`Dispatch.hs:874-882`). Identical on master, but the new `EndWild`
  abstraction makes it silent. → Doc note + regression test; `setNestedRouteFallthrough True`
  already fixes it.

- [x] **(latent) base `YesodDispatchNested (Route site)` returns `Just<404>`, never `Nothing`**
  (`Class/Dispatch.hs:74-75`), contradicting the class haddock. Only reachable from the
  intended terminal caller `toWaiAppYre`, where 404 is correct. → Fix the haddock or the
  instance to prevent a future footgun.

---

## Test coverage

- [x] **Nested-dispatch entry points are happy-path-only.** `urlToDispatch`/
  `toWaiAppPlainNested`/`toWaiAppYreNested` have one test (`NestedDispatch.hs:312-323`, matching
  shallow path). → Add: (a) a miss hitting the `Nothing -> yesodRunner notFound` branch
  (`Class/Dispatch.hs:192-193`); (b) a too-short/non-matching `pathInfo` to a *deep* fragment
  confirming a clean 404 rather than mis-dispatch through `drop parentDepth` (`Dispatch.hs:633`).

- [x] **`!`-separator `BackwardsR` is never WAI-dispatched** (only render/parse). Low risk
  (`!#` only flips compile-time overlap checking; routes like plain `/#Int`). → Optional
  dispatch test for completeness.

---

## Cleanup (parsons-haskell / matt-code-review lenses)

- [x] **Remove dead focus-mode machinery** (`Dispatch.hs`): `nrsTargetName` is only ever
  `Nothing` (`:518,:975`, literal `:261`), so `determinePhase` always yields `TopLevelDispatch`
  and the `mtargetName`/`mtargetMatch` branches (`:216-234`) + leaf Just-guard (`:303-309`) are
  unreachable. Delete `NestedRouteSettings`/`DispatchPhase`/`determinePhase` (~40 lines). The
  matching `mtarget'` in `RouteAttrs.goTree` (`:57-60`) is also always `Nothing`.
- [x] **Delete dead `routeConArity`** (`Internal.hs:85-86`, zero call sites). Collapse the
  `Arity` newtype (`:191`) — it's only a discarded `Right` payload; `checkNestedSubArity` is
  morally `Maybe ArityMismatch`.
- [x] **Finish the dedup sweep:** `mkYesodSubDispatchInstance` re-inlines `parseYesodName` +
  the `appCxt` builder (`Internal/TH.hs:357-385,276-284`) — call the existing ones;
  `mkRouteCon`/`mkRouteConsOpts` forked in `RenderRoute.hs` (`:378-420` vs `:951-1030`);
  ParentArgs unit/single/tuple idiom hand-spelled ~9× across Exp/Pat/Type — extract
  `parentArgsExpr`/`parentArgsPat`/`parentArgsType` on top of `mkTupE`; `getParentDynVars`
  (`Dispatch.hs:758-763`) decodes a `Pat` just encoded from `[Name]` — thread the `[Name]`.
- [x] **Route tests through `RuntimeHarness`:** `annotate_` is copy-pasted in
  `NestedDispatch.hs`/`FallthroughSpec.hs`; ~10 modules re-roll `testRequestIO` over
  `assertRequest` with inconsistent arg order. Add `assertGet`/`assertRequestFor app`.
- [x] **`Parse.hs` should use `fail`, not `error`** for malformed-route diagnostics in `Q`
  (`:109,117,134,…`) — the branch already converted the overlap check to `fail` (`:1042`).
- [x] **`tests` cabal component double-vision** (`yesod-core.cabal:128-129,210-211`):
  `hs-source-dirs: test, src` + `Yesod.Routes.Class`/`…RenderRoute` in `other-modules` **and**
  a `yesod-core` dependency → two copies of `RenderRouteNested`/`RouteOpts`. Compiles via
  home-package shadowing but fragile. (`test-routes` avoids it by not depending on `yesod-core`.)
- [x] **Strip leftover `-ddump-splices -ddump-to-file` pragmas** from the ~14 committed test
  modules (debugging residue).
- [x] **TH compile-time waste:** `mkNestedDispatchInstanceWith` re-passes the full root `res`,
  so `findNestedRoute` re-walks from root per node (O(N×tree)); each nested parent is
  `lookupTypeName`/`reify`/`isInstance`-probed multiple times per splice. → Resolve each
  `RouteCon` once and thread it.

---

## Verified NOT a problem (don't re-investigate)

- Inline vs nested dispatch divergence — textual only; hardcoded ops == inline `mds*`
  defaults, `toTypedContent` idempotent, `void` phantom. Suite passes 23/0.
- `thisRouteParentArgs` `<>`/`++` precedence (`Dispatch.hs:248`) — correct (all lists).
- `RouteAttrs.goRes` empty-attr elision + `WildP -> mempty` catch-all — equivalent to master.
- `mkRenderRouteClauses` subsite arm dropping multipiece — safe (subsites carry no multipiece).
- Param fallthrough runtime specs assert distinct 200/404 — not tautological.
- New `Route.*` specs are wired into both `RouteSpec.hs` and the cabal.
- Subsite-under-nested *delegated* path is covered by `WaiSubsite.hs` (incl. dynamic parent);
  deleted `RouteSpec`/`Hierarchy` dispatch assertions are re-covered by the `*Runtime` specs.
