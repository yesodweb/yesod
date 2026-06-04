# TODO — `mattp/nested-route-discovery` (round-4 review)

> **STATUS: ALL ITEMS RESOLVED.** Every finding below (#1–#16, plus the
> bench/th.hs housekeeping) has been implemented across PR-A…PR-D and the
> housekeeping commit. #7 required no action. Both suites green (311 + 77).
> See `git log` from `53b9c404` for the per-item commits. Original review text
> retained below for reference.

Findings from a max-effort review of `c331b994..HEAD` (the feature refinement:
acad9bf9 → 532d50ea → 9017bcdc → 43998c2f → 53b9c404), through the
legibility / duplication / type-safety lenses. HEAD = `53b9c404`, shipped as
**1.7.0.0**. Build + both test suites green (297 + 77).

Line numbers are against `53b9c404`.

---

## P0 — Confirmed compile regression (fix first)

### 1. `Dispatch.hs:236` — parameterized + opted-out + nested + split = compile failure

The `go` delegation probe builds `appliedRouteType typeName tyargs`, applying the
**site's** type args to a child datatype that InlineCompat generated at kind
`Type`. For a parameterized site with a nested route, default opts (opted out),
split across `mkYesodData`/`mkYesodDispatch` (so the child is in scope at the
dispatch splice), this yields the ill-kinded `SubParentR a`, fed to `isInstance`
→ `reifyInstances` **aborts the splice**. **master compiled this** (no probe), so
it is a backwards-compat regression.

**Empirically verified:**
- `isInstance` on an ill-kinded type *throws* (`[GHC-83865] Expected kind
  '* -> k0', but 'Mono' has kind '*'`), it does not return `False`. `runMaybeT`
  does not catch it — it propagates and aborts.
- Full repro (parameterized `Poly a`, default opts, nested parent, split
  data/dispatch) fails with `Expected kind 'k0 -> *', but 'SubParentR' has kind
  '*' … reifyInstances: YesodDispatchNested SubParentR a`.
- **Fix verified:** reverting the line to `fullyApplyType typeName` (saturate by
  the child's *own reified arity* — safe, `lookupTypeName` already succeeded)
  makes the repro compile. `appliedRouteType … tyargs` is only well-kinded when
  child arity == site arity, which is exactly false in InlineCompat.

**Fix:** revert `Dispatch.hs:236` to `appliedT <- Trans.lift $ fullyApplyType
typeName`, and fix the now-false comment (see #13). Repro module used for
verification: parameterized site + `defaultOpts` + nested `ResourceParent` +
separate `mkYesodData`/`mkYesodDispatch` splices.

**Not covered by any test** — the opted-in test (`ParamTopLevelRuntime`,
`setParameterizedSubroute True`) makes the child kind `Type -> Type`, so
`SubParentR a` is well-kinded; that masked the bug. **Add** a regression test:
parameterized + default opts + nested + split → must compile and dispatch.

### 2. `Core/Internal/TH.hs:329` — the friendly arity guard is dead in its own case

Same mechanism. The `isInstance` probe (`:329`) runs *before*
`checkNestedSubArity` (`:345`). When the nested datatype is in scope but
kind-mismatched — exactly the misuse `checkNestedSubArity` exists to diagnose —
`isInstance` throws the cryptic kind error first, so the friendly `fail` is never
reached. Fixing #3 (resolve once, probe by reified arity) dissolves this too.

---

## P1 — Type refactor (your primary ask — and it prevents #1/#2 by construction)

### 3. Resolve route names once; carry the result in a type (`RouteCon`)

`mname <- lookupTypeName name; case … Just/Nothing → …` recurs at **10 sites**
(Dispatch 230/668/864, RenderRoute 362/476/606/991, ParseRoute 174,
Core/Internal/TH 204/293/326). Each independently decides what `Nothing`
(same-splice / unresolved) means — and they **genuinely diverge**: the probe
treats `Nothing` as "generate," the arity check as "skip" (the author already had
to hand-hoist `mtyname` at TH.hs:326 to reconcile two consumers). The
*saturation* strategy also diverges: RenderRoute/ParseRoute/RouteAttrs probes use
`fullyApplyType` (reify the child's arity — **correct**), Dispatch/Core use
`appliedRouteType … tyargs` (the **#1 bug**).

```haskell
data RouteCon = RouteCon { rcName :: String, rcResolved :: Maybe Name }
resolveRouteCon     :: String -> Q RouteCon
nestedInstanceExists :: Name {- class -} -> RouteCon -> Q Bool  -- one probe, one rule
```

Resolve at the boundary; the "saturate by reified arity vs site tyargs" choice
lives in one tested place. #1 becomes unrepresentable. This is the highest-
leverage item: it both dedups #9 and locks in the #1 fix.

### 4. `appliedRouteType :: Name` vs `appliedRouteTypeNamed :: String` — contradictory unresolved-name policies

`Internal.hs:60`/`72`. The `Name` variant's `NoTyArgs` arm `reify`s (requires
in-scope, throws otherwise); the `String` variant *fabricates* `mkName`. Nothing
in the type says which input is safe for which call. Unify under `RouteCon`.

### 5. `mkPathPat :: Pat -> [Pat] -> Pat` — tail is too wide → `data PathTail`

`Internal.hs:143`. Only 4 tails are ever valid: `[]`, `VarP rest`, `WildP`, and
multipiece `ViewP fromPathMultiPiece (Just n)`. Now that it is the shared path-
pattern builder across ~10 parse+dispatch sites, a caller can pass any `Pat` and
silently match the wrong number of segments. `data PathTail = EndExact | EndRest
Name | EndWild | EndMulti Name`.

### 6. `checkNestedSubArity :: String -> String -> Int -> Int -> Either String ()`

`Internal.hs:96`. Two adjacent `String`s + two adjacent `Int`s are swappable with
no type error (load-bearing for message accuracy — the predicate is symmetric);
`()` on success discards the proven arity (validate, not parse); `String` error
forces the test to substring-match. → `newtype SubsiteName`/`RouteName` +
`Either ArityMismatch Arity`.

### 7. Honest note: `TyArgs`'s `NonEmpty` is decorative

`Types.hs:47`. No consumer takes its head or destructures `x :| _`; all flatten
via `tyArgsList`/`tyArgsTypes` or branch on `hasTyArgs`. Load-bearing parts are
the `(Type, Name)` pairing and the two-constructor split. Fine to leave — just
don't count `NonEmpty` as a guarantee. (No action required unless simplifying.)

---

## P2 — Duplication (your primary ask)

### 8. `mkNestedDispatchInstance` (558) vs `mkNestedSubDispatchInstance` (802) — ~80-line twins
Identical `findNestedRoute`/`preDyns`/`parentDynsP`/`genNestedDispatchClauses`/
`pathInfo`/child-recursion, differing only by the two Names that
**`NestedDispatchConfig` already carries** (`ndcDispatchFn`/`ndcDispatchClass`).
Fold into one function driven by the config; the top-level-only
`urlToDispatchInstances` becomes a config flag.

### 9. The `lookupTypeName` + `isInstance` probe — copy-pasted 6×
Dispatch 667-670 / 862-865, RenderRoute 476-482 / 606-612, ParseRoute 173-182,
Core/Internal/TH 327-330, with the `fullyApplyType`-vs-`appliedRouteType`
divergence from #3. Extract `nestedInstanceExists` — collapses all six **and**
fixes #1/#2 by construction. (The comment at `Dispatch.hs:232` admits the
consistency is "enforced" only by a comment.)

### 10. `RenderRouteNested` instance body triplicated
`RenderRoute.hs:353`, `891`, `984` — same `piecesAndNames → preDyns →
parentDynT → mkRenderRouteNestedClauses → InstanceD` sequence (~55 lines), with
the `error "empty name????"` partial appearing 3×. Extract one
`mkRenderRouteNestedInstanceFor`.

### 11. `foldl' AppE (ConE (mkName name)) dyns` — route construction repeated 7×
Dispatch 248/351/396/457/921/980, RenderRoute 863, already in two spellings
(`:980` lambda fold vs `:921` point-free). `handlePiecesM` already produces the
`[Exp]`; add `applyConPieces :: String -> [Exp] -> Exp` next to it.

### 12. Test harness `testRequestIO` duplicated 4–6×
ParamTopLevel/ParamSplitSub/ParameterizedSubDispatch/ParamSubDispatchInstance +
the two fallthrough modules, in **two disagreeing styles** (`shouldBe`-on-
`simpleStatus` vs `assertStatus`). Extract one shared harness module taking the
WAI app + a round-trip helper.

---

## P3 — Lower / minor

### 13. `Dispatch.hs:232-235` — comment is stale AND actively wrong
Cites line numbers `':675'`/`':870'` (actual 669/864) and claims "with concrete
`tyargs` it probes the same polymorphic instance head" — **false**, and is
exactly the #1 bug. Fix as part of #1; reference function names, not line numbers.

### 14. `error` → `fail` in `Q` generators
`RenderRoute.hs:413/559/701/712` (`"empty name????"`, `"mkPieces 120"`),
`Core/Internal/TH.hs:289` (`"Bad context"`), `Dispatch.hs:716` (empty `error ""`
in dead `mkUrlToDispatchNestedInstance`). `fail` gives an attributable splice
location. `:413` is faintly reachable (dynamic-piece type whose `show` has no
alphanumerics); the rest are defensive.

### 15. `ParseRoute.hs` mixes two AST dialects
The pure `buildInlineParseClauses` builds tuples/bodies with plain constructors
(`TupP`/`mkTupE`/`LamE`); sibling `generateParseRouteClause` (123/133/162) does
the identical shapes via `liftQ [p| … |]`/`[e| … |]` quotes. Pick one — a
tuple-shape change otherwise needs mirroring in two dialects.

### 16. (PLAUSIBLE, untested) `Dispatch.hs:1005` — no-fallthrough nested-instance asymmetry
The inline no-fallthrough path (`:287-290`) converts a child's `Nothing` into a
404 handler call; the delegated nested-instance path (`:1005`) returns
`nestedCall` (a `Maybe`) directly with no 404 conversion. A cross-module split
where a no-fallthrough parent delegates to a child instance that returns
`Nothing` would fall through to siblings instead of committing to 404. Narrow,
untested — verify with a split test before fixing, or make `:1003-1005` mirror
the inline `Nothing → 404` shape.

---

## Already resolved this cycle (no action)
- Dedup sweep #1-#3 from round 3 (handlePieceM/mkPathPat/appliedRouteTypeNamed
  centralized), disabled-fallthrough test (`ParamNoFallthroughRuntime` — verified
  non-vacuous), parse-spec hardening, ChangeLog + version (1.7.0.0) + `@since`
  reconciliation — all landed in `43998c2f`/`53b9c404`.
- Test wiring (suite aggregator + cabal `other-modules`) verified complete.
- Helper extractions verified behavior-preserving (name-allocation order,
  tail-pattern shapes, `delegatingBody`).

## Housekeeping (carried)
- `bench/th.hs` — still orphaned (not a cabal target) and bitrotted (imports a
  nonexistent module, stale `mkDispatchClause` arity, `mkRouteConsOpts … [] []`
  pre-`TyArgs`). Migrate-or-delete; maintainer call.

---

## Suggested batching
1. **PR-A (urgent):** #1 fix + #13 comment + the missing regression test. One-line
   code change, verified.
2. **PR-B (type + dedup):** #3 `RouteCon` + #9 `nestedInstanceExists` together
   (the refactor and the dedup are the same change), then #4. Dissolves #2.
3. **PR-C (dedup):** #8, #10, #11, #12.
4. **PR-D (polish):** #5 `PathTail`, #6 `checkNestedSubArity` types, #14 `fail`,
   #15 dialect, #16 (after a split test).
