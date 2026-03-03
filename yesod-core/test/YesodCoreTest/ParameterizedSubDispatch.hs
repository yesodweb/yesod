{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test mkYesodSubDispatch with parameterized/constrained types.
-- This exercises the Dispatch TH code path for parameterized subsites.
--
-- The route data is defined in ParameterizedSubDispatch.Data (via
-- mkYesodSubData) and the dispatch instance is here (via
-- mkYesodSubDispatch) due to GHC stage restrictions.
module YesodCoreTest.ParameterizedSubDispatch where

import Yesod.Core
import Data.Text (Text)
import YesodCoreTest.ParameterizedSubDispatch.Data

getParamDispHomeR :: SubHandlerFor (ParamSubDispatch subsite) master Text
getParamDispHomeR = pure "home"

getParamDispItemR :: Int -> SubHandlerFor (ParamSubDispatch subsite) master Text
getParamDispItemR _ = pure "item"

instance ParamSubDispatchClass subsite master => YesodSubDispatch (ParamSubDispatch subsite) master where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesParamSubDispatch)
