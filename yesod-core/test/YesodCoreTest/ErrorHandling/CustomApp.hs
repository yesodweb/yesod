{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | a custom app that throws an exception
module YesodCoreTest.ErrorHandling.CustomApp
    (CustomApp(..)
    , MyException(..)

    -- * unused
    , Widget
    , resourcesCustomApp
    ) where


import Yesod.Core.Types
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
  catchBehavior _ exception =
    case E.fromException exception of
      Just MkMyException -> Rethrow
      Nothing -> Catch
