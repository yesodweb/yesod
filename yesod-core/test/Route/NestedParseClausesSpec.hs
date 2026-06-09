-- | Unit tests for 'generateParseRouteClause' run in 'PureQ' — the
-- monad-polymorphic (over 'Quote') nested @parseRouteNested@ clause generator.
-- Production runs it in 'Q' (hygienic 'newName'); here we pin its name supply to
-- a deterministic counter so its 'Clause' output — and the set of parent names
-- it requests a 'ParseRouteNested' instance for — can be asserted directly, with
-- no splicing or 'runQ'. This is the payoff of hoisting the @isInstance@ query
-- out of the generator (see 'generateParseRouteClause').
module Route.NestedParseClausesSpec (spec) where

import Test.Hspec
import Control.Monad.State.Strict (runStateT)
import qualified Data.Set as Set
import Data.Set (Set)
import Language.Haskell.TH
import Yesod.Routes.TH.Types
import Yesod.Routes.TH.RenderRoute (defaultOpts)
import Yesod.Routes.TH.ParseRoute (generateParseRouteClause)
import Route.PureQ (runPureQ)

-- | Run the nested clause generator in 'PureQ': deterministic fresh names, pure
-- output. Returns the clause plus the set of parent names it requested a
-- 'ParseRouteNested' instance for.
run :: Set String -> ResourceTree Type -> (Clause, Set String)
run existing t =
    runPureQ $
        runStateT (generateParseRouteClause existing defaultOpts t) mempty

leaf :: String -> [Piece Type] -> Dispatch Type -> ResourceTree Type
leaf name pieces d = ResourceLeaf (Resource name pieces d [] True)

methods :: [String] -> Dispatch Type
methods = Methods Nothing

parent :: String -> [Piece Type] -> [ResourceTree Type] -> ResourceTree Type
parent name pieces = ResourceParent name True mempty pieces

fooParent :: ResourceTree Type
fooParent =
    parent "FooR" [Static "foo"]
        [ leaf "BarR" [Static "bar"] (methods ["GET"]) ]

spec :: Spec
spec = describe "generateParseRouteClause (nested parseRoute codegen in PureQ)" $ do
    it "records a parent that has no nested instance yet" $
        snd (run mempty fooParent) `shouldBe` Set.singleton "FooR"

    it "does not record a parent that already has a nested instance (hoisted query)" $
        snd (run (Set.singleton "FooR") fooParent) `shouldBe` mempty

    it "delegates a parent clause through parseRouteNested" $
        pprint (fst (run mempty fooParent)) `shouldContain` "parseRouteNested"

    it "records nothing for a bare leaf" $
        snd (run mempty (leaf "BarR" [Static "bar"] (methods ["GET"]))) `shouldBe` mempty

    it "is reproducible: re-running the same tree yields an identical clause (names and all)" $
        -- The PureQ counter resets to 0 each run, so the deterministic binders
        -- make the whole Clause '=='. Under Q's 'newName' the uniques would
        -- differ between runs — this equality is exactly what going through the
        -- pure 'Quote' instance buys the tests.
        run mempty fooParent `shouldBe` run mempty fooParent
