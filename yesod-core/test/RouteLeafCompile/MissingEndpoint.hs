{-# LANGUAGE TypeFamilies #-}
module MissingEndpoint where
import Yesod.Core.RouteLeaf
import YesodCoreTest.RouteLeafHook.Foundation

bad :: RouteLeaves AccountR -> ()
bad (LeafItemR _) = ()
