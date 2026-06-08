{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Bug-2 regression fixture: @InnerR@ is split out into its own focused
-- module (so a @RouteAttrsNested InnerR@ instance is in scope), while its
-- parent @OuterR@ is /not/ split — the main @App@ module inlines @OuterR@.
--
-- When the full @RouteAttrs App@ instance descends through the inlined
-- @OuterR@ and reaches the delegated @InnerR@, the generated @routeAttrs@
-- clause must apply the accumulated ancestor pattern (@front@), producing
-- @routeAttrs (OuterR (InnerR _ x)) = routeAttrsNested x@. Dropping @front@
-- (the bug) would emit @routeAttrs (InnerR x) = …@, which fails to compile
-- because @Route App@ has no top-level @InnerR@ constructor.
module YesodCoreTest.NestedDispatch.InnerR where

import Data.Text (Text)
import YesodCoreTest.NestedDispatch.Resources
import Yesod.Core

mkYesodOpts (setFocusOnNestedRoute (Just "InnerR") defaultOpts) "App" nestedDispatchResources

getInnerIndexR :: HandlerFor App Text
getInnerIndexR = pure "getInnerIndexR"
