{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Shared scaffolding for the @setUrlToDispatchSiteHook@ test. The hook is a
-- general @url -> site -> site@ primitive; here we exercise it with a
-- class-constrained hook ('recordDispatchHook') that mirrors the shape of the
-- per-route authorization install it was designed for: it resolves a
-- 'DispatchHookRoute' instance for the fragment route value the generated
-- nested-dispatch clause hands it, and records a label into the site so a
-- handler can observe both that the hook fired and which route value (with its
-- parent args) it captured.
module YesodCoreTest.DispatchSiteHook.Common where

import Yesod.Core
import Yesod.Routes.TH.Types (ResourceTree)
import Data.Text (Text)

-- | A site whose 'hookAppLabel' is set by the dispatch-site hook. Starts as
-- 'Nothing'; a handler reporting anything other than @\"NO-HOOK\"@ proves the
-- hook ran and replaced the site before the handler executed.
data HookApp = HookApp { hookAppLabel :: Maybe Text }

-- | Class-constrained target for the dispatch-site hook. The fact that the
-- generated clause demands an instance for the fragment's 'WithParentArgs'
-- type is what makes per-route resolution (e.g. authorization) possible.
class DispatchHookRoute url where
    dispatchHookLabel :: url -> Text

-- | The hook installed via 'setUrlToDispatchSiteHook'. Shape: @url -> site -> site@.
recordDispatchHook :: DispatchHookRoute url => url -> HookApp -> HookApp
recordDispatchHook url app = app { hookAppLabel = Just (dispatchHookLabel url) }

-- | Read whatever label the hook installed (or @\"NO-HOOK\"@ if it did not run).
currentLabel :: HandlerFor HookApp Text
currentLabel = maybe "NO-HOOK" id . hookAppLabel <$> getYesod

hookResources :: [ResourceTree String]
hookResources = [parseRoutes|
/widgets WidgetsR:
    / WidgetsIndexR GET

/orgs/#Int OrgR:
    / OrgIndexR GET
|]
