{-# LANGUAGE TypeFamilies #-}
module Yesod.Contrib.Persist
    ( YesodPersist (..)
    , Persist (..)
    ) where

import Yesod.Handler
import Yesod.Yesod
import Database.Persist

class YesodPersist y where
    type YesodDB y :: (* -> *) -> * -> *
    runDB :: YesodDB y (GHandler sub y) a -> GHandler sub y a
