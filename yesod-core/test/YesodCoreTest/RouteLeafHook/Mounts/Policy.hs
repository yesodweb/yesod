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
        PlainR -> recordMountEvent "plain-policy"
        RootMountR capture selected -> mountPolicy (capture == 7) selected
        WrongMountR _ -> recordMountEvent "wrong-policy"
        MountGroupR _ _ -> error "nested dispatch must own its policy"

instance AuthorizeRoute (MountGroupR a) where
    authorizeRoute parent (NestedMountR capture selected) =
        mountPolicy (parent == 42 && capture == "allowed") selected

mountPolicy :: Bool -> Route MountSub -> HandlerFor (MountApp a) ()
mountPolicy allowed selected = do
    recordMountEvent $ case selected of
        SubPageR -> "mount-page"
        SubErrorR -> "mount-error"
    if allowed
        then setSession "mount-policy" "authorized"
        else permissionDenied "mount denied"
