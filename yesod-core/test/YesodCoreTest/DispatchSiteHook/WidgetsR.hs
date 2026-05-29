{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

-- | Focused dispatch for the no-parent-args route @WidgetsR@, with the
-- dispatch-site hook installed. The hook is wired in via
-- 'setUrlToDispatchSiteHook' on the focused route options only.
module YesodCoreTest.DispatchSiteHook.WidgetsR where

import Data.Text (Text)
import Yesod.Core
import YesodCoreTest.DispatchSiteHook.Common

mkYesodOpts
    (setUrlToDispatchSiteHook 'recordDispatchHook
        (setFocusOnNestedRoute (Just "WidgetsR") defaultOpts))
    "HookApp"
    hookResources

-- | A no-parent-args route carries @()@ parent args.
instance DispatchHookRoute (WithParentArgs WidgetsR) where
    dispatchHookLabel (WithParentArgs () _frag) = "widgets"

getWidgetsIndexR :: HandlerFor HookApp Text
getWidgetsIndexR = currentLabel
