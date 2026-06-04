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
import Language.Haskell.TH
import Yesod.Routes.TH.Types
import Yesod.Routes.TH.ParseRoute (buildInlineParseClauses)

-- | Run the pure builder at the top level (no accumulated parent pieces, no
-- wrapping), the way 'generateParseRouteClausesInline' invokes it.
run :: ResourceTree Type -> [Clause]
run t = evalState (buildInlineParseClauses [] id t) 0

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
