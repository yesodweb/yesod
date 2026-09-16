{-# LANGUAGE TypeFamilies #-}
module MissingEndpoint where
import Yesod.Core.RouteLeaf
import YesodCoreTest.RouteLeaf.Foundation

bad :: RouteLeaves AccountR -> Bool
bad (LeafItemR _) = True
