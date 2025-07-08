{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | a custom app that throws an exception
module YesodCoreTest.ErrorHandling.CustomApp
    (CustomApp(..)
    , MyException(..)

    -- * unused
    , Widget
    , resourcesCustomApp
    ) where

import Yesod.Core
import qualified UnliftIO.Exception as E

data CustomApp = CustomApp

mkYesod "CustomApp" [parseRoutes|
/throw-custom-exception CustomHomeR GET
|]

getCustomHomeR :: Handler Html
getCustomHomeR =
  E.throwIO MkMyException

data MyException = MkMyException
 deriving (Show, E.Exception)

instance Yesod CustomApp where
  -- something we couldn't do before, rethrow custom exceptions
  catchHandlerExceptions _ action handler =
    action `E.catch` \exception -> do
      case E.fromException exception of
        Just MkMyException -> E.throwIO MkMyException
        Nothing -> handler exception
