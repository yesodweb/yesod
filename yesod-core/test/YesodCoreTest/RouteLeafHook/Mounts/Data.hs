{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.RouteLeafHook.Mounts.Data where

import Data.IORef
import Data.Text (Text)
import Yesod.Core
import Yesod.Core.Types (YesodSubRunnerEnv (..))
import YesodCoreTest.RouteLeafHook.Options

data MountSub = MountSub (IORef [String])

mkYesodSubData "MountSub" [parseRoutes|
/page SubPageR GET
/error SubErrorR GET
|]

instance Yesod parent => YesodSubDispatch MountSub parent where
    yesodSubDispatch = $(mkYesodSubDispatch [parseRoutes|
/page SubPageR GET
/error SubErrorR GET
|])

getSubPageR :: Yesod parent => SubHandlerFor MountSub parent Text
getSubPageR = do
    MountSub events <- getSubYesod
    liftIO $ modifyIORef' events (++ ["handler"])
    value <- lookupSession "mount-policy"
    pure $ maybe "missing policy session" id value

getSubErrorR :: Yesod parent => SubHandlerFor MountSub parent Text
getSubErrorR = invalidArgs ["subsite handler error"]

data MountApp a = MountApp (IORef [String])

data WrongRouteSub = WrongRouteSub

instance RenderRoute WrongRouteSub where
    data Route WrongRouteSub = WrongRouteR deriving (Eq, Show, Read)
    renderRoute WrongRouteR = ([], [])

instance ParseRoute WrongRouteSub where
    parseRoute ([], _) = Just WrongRouteR
    parseRoute _ = Nothing

mkYesodDataOpts (setParameterizedSubroute True hookRouteOpts) "MountApp a" [parseRoutes|
/mount/#Int RootMountR MountSub getRootSub
/group/#Int MountGroupR:
    /mount/#Text NestedMountR MountSub getNestedSub
/plain PlainR GET
/wrong WrongMountR WrongRouteSub getWrongSub
|]

-- A deliberately inconsistent handwritten subsite: a Just route outside the
-- mount must not be mistaken for a miss or passed to a different policy.
instance YesodSubDispatch WrongRouteSub (MountApp a) where
    yesodSubDispatch env = ysreParentRunner env
        (pure $ toTypedContent ("unexpected handler" :: Text))
        (ysreParentEnv env) (Just PlainR)

getWrongSub :: MountApp a -> WrongRouteSub
getWrongSub _ = WrongRouteSub

getRootSub :: MountApp a -> Int -> MountSub
getRootSub (MountApp events) _ = MountSub events

getNestedSub :: MountApp a -> Int -> Text -> MountSub
getNestedSub (MountApp events) _ _ = MountSub events

recordMountEvent :: String -> HandlerFor (MountApp a) ()
recordMountEvent event = do
    MountApp events <- getYesod
    liftIO $ modifyIORef' events (++ [event])

instance Yesod (MountApp a) where
    messageLoggerSource = mempty
    makeSessionBackend _ = pure Nothing
    yesodMiddleware handler = do
        recordMountEvent "middleware"
        defaultYesodMiddleware handler
    errorHandler err = recordMountEvent "error" >> defaultErrorHandler err
