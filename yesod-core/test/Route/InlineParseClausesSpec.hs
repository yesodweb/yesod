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

-- | Run the pure builder at the top level (no accumulated parent pieces, no
-- wrapping), the way 'generateParseRouteClausesInline' invokes it.
run :: ResourceTree Type -> [Clause]
run t = evalState (buildInlineParseClauses [] id t) (0 :: Int)

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

    it "flattens a parent into one clause per descendant leaf (no clause for the parent itself)" $ do
        let tree = parent "AdminR" [Static "admin"]
                [ leaf "UsersR" [Static "users"] (methods ["GET"])
                , leaf "PostsR" [Static "posts"] (methods ["GET"])
                ]
        length (run tree) `shouldBe` 2

    it "flattens deeply-nested parents (one clause per leaf, parents contribute none)" $ do
        let tree = parent "AR" [Static "a"]
                [ parent "BR" [Static "b"]
                    [ leaf "CR" [Static "c"] (methods ["GET"])
                    , leaf "DR" [Static "d"] (methods ["GET"])
                    ]
                ]
        length (run tree) `shouldBe` 2

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
