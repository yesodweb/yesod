{-# LANGUAGE QuasiQuotes #-}

module Route.RouteAttrSpec (spec) where

import Yesod.Core
import Test.Hspec
import Yesod.Routes.TH.Types
import Data.Set (Set)
import qualified Data.Set as Set

routeNoAttributes :: [ResourceTree String]
routeNoAttributes = [parseRoutes|
/one OneR:
  /two TwoR:
    /three ThreeR:
      /four FourR POST
  |]

routeWithAttributes :: [ResourceTree String]
routeWithAttributes = [parseRoutes|
/one OneR:
  /two TwoR !x !z:
    /three ThreeR !y:
      /four FourR POST
  |]

spec :: Spec
spec = do
    describe "route attrs present" $ do
        it "has no route attrs on parent" $ do
            let parentDetails = concat $ frParentDetails <$> flatten routeNoAttributes
            let attrs = (\detail -> (pdName detail, pdAttrs detail)) <$> parentDetails
            Set.fromList attrs
                `shouldBe`
                    Set.fromList 
                        [ ("OneR", Set.empty)
                        , ("TwoR", Set.empty)
                        , ("ThreeR", Set.empty)
                        ]
        it "has route attrs on parent" $ do
            let parentDetails = concat $ frParentDetails <$> flatten routeWithAttributes
            let attrs = (\detail -> (pdName detail, pdAttrs detail)) <$> parentDetails
            Set.fromList attrs
                `shouldBe`
                    Set.fromList 
                        [ ("OneR", Set.empty)
                        , ("TwoR", Set.fromList ["x", "z"])
                        , ("ThreeR", Set.singleton "y")
                        ]
