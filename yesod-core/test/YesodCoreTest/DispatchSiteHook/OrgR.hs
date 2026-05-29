{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

-- | Focused dispatch for the parent-args route @OrgR@ (@\/orgs\/#Int@), with the
-- dispatch-site hook installed. This is the load-bearing case: the hook must
-- receive the parent @Int@ alongside the fragment, so that per-route
-- authorization can see the dynamic parent argument (e.g. the org id).
module YesodCoreTest.DispatchSiteHook.OrgR where

import Data.Text (Text)
import qualified Data.Text as Text
import Yesod.Core
import YesodCoreTest.DispatchSiteHook.Common

mkYesodOpts
    (setUrlToDispatchSiteHook 'recordDispatchHook
        (setFocusOnNestedRoute (Just "OrgR") defaultOpts))
    "HookApp"
    hookResources

-- | The parent @Int@ (org id) is threaded into the 'WithParentArgs' value the
-- hook receives, so the label can reflect it.
instance DispatchHookRoute (WithParentArgs OrgR) where
    dispatchHookLabel (WithParentArgs orgId _frag) = "org:" <> Text.pack (show orgId)

getOrgIndexR :: Int -> HandlerFor HookApp Text
getOrgIndexR _orgId = currentLabel
