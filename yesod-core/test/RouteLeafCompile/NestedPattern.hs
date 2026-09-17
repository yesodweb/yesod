{-# LANGUAGE TypeFamilies #-}
module NestedPattern where
import Yesod.Core.RouteLeaf
import YesodCoreTest.RouteLeafHook.Foundation

bad :: RouteLeaves OrgR -> ()
bad (AccountR _ _) = ()
