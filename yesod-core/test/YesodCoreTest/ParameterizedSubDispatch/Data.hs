{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

-- | Route data for a parameterized subsite used by
-- ParameterizedSubDispatch to test mkYesodSubDispatch.
module YesodCoreTest.ParameterizedSubDispatch.Data where

import Yesod.Core

class ParamSubDispatchClass subsite master | subsite -> master where
  getDispatchValue :: subsite -> master -> String

data ParamSubDispatch subsite = ParamSubDispatch subsite

mkYesodSubData "(ParamSubDispatchClass subsite master) => ParamSubDispatch subsite" [parseRoutes|
/ ParamDispHomeR GET
/item/#Int ParamDispItemR GET
|]
