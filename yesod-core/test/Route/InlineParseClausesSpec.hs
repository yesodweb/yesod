{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Unit tests for 'buildInlineParseClauses', the pure (effect-free) core of
-- the backwards-compatible inline @parseRoute@ codegen. Because it is pure —
-- it takes a deterministic fresh-name supply instead of 'Q'\'s 'newName' and
-- builds AST directly rather than through quotation brackets — its '[Clause]'
-- output can be asserted on directly, with no splicing, compilation, or
-- 'runQ'. This is the testability win from extracting the pure builder out of
-- 'generateParseRouteClausesInline'.
module Route.InlineParseClausesSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (evalState)
import Data.Maybe (catMaybes)
import Language.Haskell.TH
import Web.PathPieces (fromPathPiece)
import Yesod.Routes.TH.Types
import Yesod.Routes.TH.ParseRoute (buildInlineParseClauses)

-- | Run the pure builder at the top level (no wrapping), the way
-- 'generateParseRouteClausesInline' invokes it.
run :: ResourceTree Type -> [Clause]
run t = evalState (buildInlineParseClauses id t) (0 :: Int)

leaf :: String -> [Piece Type] -> Dispatch Type -> ResourceTree Type
leaf name pieces d = ResourceLeaf (Resource name pieces d [] True)

methods :: [String] -> Dispatch Type
methods = Methods Nothing

parent :: String -> [Piece Type] -> [ResourceTree Type] -> ResourceTree Type
parent name pieces = ResourceParent name True mempty pieces

-- | Assert there is exactly one clause and hand it to the continuation.
withOnlyClause :: [Clause] -> (Clause -> Expectation) -> Expectation
withOnlyClause cs k =
    case cs of
        [c] -> k c
        _   -> expectationFailure $ "expected exactly one clause, got " <> show (length cs)

-- | Variables bound by a @fromPathPiece@ view pattern, descending through the
-- list\/tuple\/constructor patterns the inline builder nests them in.
boundDynVars :: Pat -> [Name]
boundDynVars (ViewP (VarE f) inner)
    | f == 'fromPathPiece = simpleVars inner
boundDynVars p = concatMap boundDynVars (subPats p)

-- | Plain 'VarP' names anywhere within a pattern.
simpleVars :: Pat -> [Name]
simpleVars (VarP n) = [n]
simpleVars p        = concatMap simpleVars (subPats p)

-- | The immediate sub-patterns of a pattern (only the shapes the inline
-- builder produces need handling).
subPats :: Pat -> [Pat]
subPats (TupP ps)     = ps
subPats (ListP ps)    = ps
subPats (ViewP _ p)   = [p]
subPats (ParensP p)   = [p]
subPats (BangP p)     = [p]
subPats (TildeP p)    = [p]
subPats (AsP _ p)     = [p]
subPats (SigP p _)    = [p]
#if MIN_VERSION_template_haskell(2,18,0)
subPats (ConP _ _ ps) = ps
#else
subPats (ConP _ ps)   = ps
#endif
subPats _             = []

-- | The variables referenced in an expression.
usedVars :: Exp -> [Name]
usedVars (VarE n) = [n]
usedVars e        = concatMap usedVars (subExps e)

-- | The match alternatives of the top-level expression, when it is a @case@
-- (the shape a parent's committing clause produces). 'Nothing' otherwise.
caseMatches :: Exp -> Maybe [Match]
caseMatches (CaseE _ ms) = Just ms
caseMatches _            = Nothing

-- | The body expression of a clause that has no @where@ bindings.
clauseExp :: Clause -> Exp
clauseExp (Clause _ body _) = clauseBodyExp body

-- | Whether a match alternative is the catch-all @_ -> Nothing@ fallback.
isNothingFallback :: Match -> Bool
isNothingFallback (Match WildP (NormalB (ConE n)) []) = n == 'Nothing
isNothingFallback _                                    = False

-- | The immediate sub-expressions of an expression.
subExps :: Exp -> [Exp]
subExps (AppE a b)       = [a, b]
subExps (AppTypeE e _)   = [e]
subExps (InfixE ma _ mb) = catMaybes [ma, mb]
subExps (UInfixE a b c)  = [a, b, c]
subExps (ParensE e)      = [e]
subExps (LamE _ e)       = [e]
subExps (SigE e _)       = [e]
subExps (CondE a b c)    = [a, b, c]
subExps (CaseE e ms)     = e : [me | Match _ (NormalB me) _ <- ms]
subExps (ListE es)       = es
#if MIN_VERSION_template_haskell(2,16,0)
subExps (TupE mes)       = catMaybes mes
#else
subExps (TupE es)        = es
#endif
subExps _                = []

-- | The expression in a clause body (the inline builder emits 'NormalB' for
-- leaves and a single-guard 'GuardedB' under fallthrough).
clauseBodyExp :: Body -> Exp
clauseBodyExp (NormalB e)             = e
clauseBodyExp (GuardedB ((_, e) : _)) = e
-- Total on the (unreached) empty-guard case: a var-free sentinel so a stray
-- input fails an assertion locally instead of aborting the whole spec run.
clauseBodyExp (GuardedB [])           = ConE '()

spec :: Spec
spec = describe "buildInlineParseClauses (pure inline parseRoute codegen)" $ do
    it "produces one clause for a single static leaf" $
        length (run (leaf "HomeR" [Static "home"] (methods ["GET"]))) `shouldBe` 1

    it "emits exactly one (committing) clause per parent, matching its prefix once" $ do
        -- 1.6 semantics: the parent contributes ONE top-level clause that
        -- matches the parent prefix, then cases over its children. It does not
        -- expand to one flat clause per descendant leaf (which would re-parse
        -- the prefix and let a child miss fall through to a sibling route).
        let tree = parent "AdminR" [Static "admin"]
                [ leaf "UsersR" [Static "users"] (methods ["GET"])
                , leaf "PostsR" [Static "posts"] (methods ["GET"])
                ]
        withOnlyClause (run tree) $ \c ->
            case caseMatches (clauseExp c) of
                Nothing -> expectationFailure "expected the parent clause body to be a case over children"
                -- two children + the committing Nothing fallback
                Just ms -> length ms `shouldBe` 3

    it "ends the parent's case with a Nothing fallback (commit-on-parent-prefix)" $ do
        -- This is the heart of the backwards-compat fix: once the parent prefix
        -- matches, a child miss must resolve to 'Nothing' (a 404 that agrees
        -- with dispatch), NOT fall through to a later top-level route.
        let tree = parent "AdminR" [Static "admin"]
                [ leaf "UsersR" [Static "users"] (methods ["GET"]) ]
        withOnlyClause (run tree) $ \c ->
            case caseMatches (clauseExp c) of
                Nothing -> expectationFailure "expected the parent clause body to be a case over children"
                Just ms -> case reverse ms of
                    (lastM : _) -> isNothingFallback lastM `shouldBe` True
                    []          -> expectationFailure "expected at least the Nothing fallback"

    it "commits at each level for deeply-nested parents (nested cases, each Nothing-terminated)" $ do
        let tree = parent "AR" [Static "a"]
                [ parent "BR" [Static "b"]
                    [ leaf "CR" [Static "c"] (methods ["GET"])
                    , leaf "DR" [Static "d"] (methods ["GET"])
                    ]
                ]
        -- One top-level clause for AR; its case has one child match (BR's
        -- committing clause-as-match) plus the Nothing fallback.
        withOnlyClause (run tree) $ \c ->
            case caseMatches (clauseExp c) of
                Nothing -> expectationFailure "expected AR's body to be a case"
                Just msA -> do
                    length msA `shouldBe` 2
                    isNothingFallback (last msA) `shouldBe` True
                    -- The non-fallback alternative is BR's committing match,
                    -- whose body is itself a case (over CR/DR) ending in Nothing.
                    case msA of
                        (Match _ (NormalB innerE) [] : _) ->
                            case caseMatches innerE of
                                Nothing  -> expectationFailure "expected BR's body to be a case"
                                Just msB -> do
                                    length msB `shouldBe` 3 -- CR, DR, Nothing
                                    isNothingFallback (last msB) `shouldBe` True
                        _ -> expectationFailure "unexpected shape for BR's match alternative"

    it "binds a dynamic piece with a fromPathPiece view pattern" $
        withOnlyClause (run (leaf "UserR" [Static "user", Dynamic (ConT ''Int)] (methods ["GET"]))) $ \c ->
            pprint c `shouldContain` "fromPathPiece"

    it "uses fromPathMultiPiece for a multipiece leaf" $
        withOnlyClause (run (leaf "FilesR" [Static "files"] (Methods (Just (ConT ''String)) ["GET"]))) $ \c ->
            pprint c `shouldContain` "fromPathMultiPiece"

    it "delegates a subsite leaf through parseRoute" $
        withOnlyClause (run (leaf "SubR" [Static "sub"] (Subsite (ConT (mkName "SubSite")) "getSub"))) $ \c ->
            pprint c `shouldContain` "parseRoute"

    it "prefixes accumulated parent pieces onto each descendant clause" $ do
        -- The parent's static piece must appear in the (single) child clause,
        -- alongside the child's own piece — that is what \"inline\" means.
        let tree = parent "OrgR" [Static "org", Dynamic (ConT ''Int)]
                [ leaf "TeamR" [Static "team"] (methods ["GET"]) ]
        withOnlyClause (run tree) $ \c -> do
            pprint c `shouldContain` "org"
            pprint c `shouldContain` "team"

    it "threads the parent's dynamic binder into the reconstructed route body" $ do
        -- A parent with a dynamic piece: its binder must be matched in the path
        -- pattern (via 'fromPathPiece') *and* fed back into the reconstructed
        -- route in the body. That threading — not merely that the pieces show
        -- up textually — is the property the inline arm has to preserve.
        let tree = parent "OrgR" [Static "org", Dynamic (ConT ''Int)]
                [ leaf "TeamR" [Static "team"] (methods ["GET"]) ]
        withOnlyClause (run tree) $ \(Clause pats body _) -> do
            let dynVars = concatMap boundDynVars pats
                used    = usedVars (clauseBodyExp body)
            -- Exactly one dynamic piece (the parent's, bound via fromPathPiece)
            -- and it is referenced in the reconstructed body — i.e. one 'True'.
            -- Empty dynVars gives @[] /= [True]@, so this never passes vacuously.
            map (`elem` used) dynVars `shouldBe` [True]
