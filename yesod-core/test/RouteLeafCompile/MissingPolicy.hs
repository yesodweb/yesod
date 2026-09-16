{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module MissingPolicy where
import Yesod.Core.RouteLeaf
import YesodCoreTest.RouteLeaf.Foundation
import YesodCoreTest.RouteLeaf.Policy

-- No concrete authorizers are imported. The generic dictionary must demand
-- all endpoint-owner instances when it is instantiated here.
bad :: ()
bad = withRouteLeaf @AuthorizeRoute OpenR (\_ _ -> ())
