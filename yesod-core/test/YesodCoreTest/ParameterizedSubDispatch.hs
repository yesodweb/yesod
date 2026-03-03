{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}
-- ^ Useful for debugging generated code; can be removed once stable

module YesodCoreTest.ParameterizedSubDispatch where

import Yesod.Core
import YesodCoreTest.ParameterizedSubData

instance
  ( ParamSubsiteClass subsite master
  ) => YesodSubDispatch (ParamSubsite subsite) master where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesParamSubsite)
