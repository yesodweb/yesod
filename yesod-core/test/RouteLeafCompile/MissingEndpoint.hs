{-# LANGUAGE TypeFamilies #-}
module MissingEndpoint where
import Yesod.Core.RouteLeaf
import YesodCoreTest.RouteLeaf.Foundation

bad :: AuthDispatch AccountR -> Bool
bad (AuthItemR _) = True
