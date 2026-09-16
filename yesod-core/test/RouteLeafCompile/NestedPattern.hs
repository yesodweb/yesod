{-# LANGUAGE TypeFamilies #-}
module NestedPattern where
import Yesod.Core.RouteLeaf
import YesodCoreTest.RouteLeaf.Foundation

bad :: RouteLeaf OrgR -> Bool
bad (DelegationR _) = True
