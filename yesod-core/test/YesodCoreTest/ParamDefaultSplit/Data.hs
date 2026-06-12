{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Route data for a delegation-probe regression: a *parameterized* top-level site with a
-- nested route, generated with /default/ opts (i.e. NOT
-- 'setParameterizedSubroute'), so the nested datatype 'SubParentDR' is
-- generated at kind 'Type'. The matching dispatch splice lives in the
-- separately compiled "YesodCoreTest.ParamDefaultSplit.Runtime", so the child
-- datatype is in scope there — the precise shape that made the @go@ delegation
-- probe build the ill-kinded @SubParentDR a@ and abort the splice.
module YesodCoreTest.ParamDefaultSplit.Data where

import Yesod.Core

data PolyD a = PolyD a

mkYesodData "PolyD a" [parseRoutes|
/ HomeDR GET
/item/#Int ItemDR GET
/sub SubParentDR:
    / SubHomeDR GET
    /detail/#Int SubDetailDR GET
|]
