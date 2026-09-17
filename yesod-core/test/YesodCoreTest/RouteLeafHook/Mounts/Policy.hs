{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module YesodCoreTest.RouteLeafHook.Mounts.Policy () where

import Yesod.Core
import YesodCoreTest.RouteLeafHook.Mounts.Data
import YesodCoreTest.RouteLeafHook.Options

instance AuthorizeRoute (Route (MountApp a)) where
    authorizeRoute () leaf = case leaf of
        LeafPlainR -> recordMountEvent "plain-policy"
        LeafRootMountR capture selected -> mountPolicy (capture == 7) selected
        LeafWrongMountR _ -> recordMountEvent "wrong-policy"

instance AuthorizeRoute (MountGroupR a) where
    authorizeRoute parent (LeafNestedMountR capture selected) =
        mountPolicy (parent == 42 && capture == "allowed") selected

mountPolicy :: Bool -> Maybe (Route MountSub) -> HandlerFor (MountApp a) ()
mountPolicy allowed selected = do
    recordMountEvent $ case selected of
        Nothing -> "mount-miss"
        Just SubPageR -> "mount-page"
        Just SubErrorR -> "mount-error"
    if allowed
        then setSession "mount-policy" "authorized"
        else permissionDenied "mount denied"
