{-# LANGUAGE TemplateHaskell #-}

-- | Shared fixtures for "Route.MissingFocusTargetSpec". The resource tree and
-- bogus type live here (a separate module) so that the spec can use them inside
-- top-level TH splices without tripping the GHC stage restriction.
module Route.MissingFocusTargetTypes
    ( sampleTree
    , bogusType
    , bogusName
    ) where

import Language.Haskell.TH.Syntax (Type(ConT), mkName)
import Yesod.Routes.TH.Types

-- A tiny tree with a single real nested parent ("ParentR"). The generators
-- under test are never handed that name; they get 'bogusName', which is absent.
sampleTree :: [ResourceTree Type]
sampleTree =
    [ ResourceParent "ParentR" True mempty [Static "parent"]
        [ ResourceLeaf $
            Resource "ChildR" [] (Methods Nothing ["GET"]) ["child"] True
        ]
    ]

bogusName :: String
bogusName = "DefinitelyNotARealRouteR"

bogusType :: Type
bogusType = ConT (mkName bogusName)
