{-# LANGUAGE TypeFamilies #-}
module Yesod.Contrib.Persist where

import Yesod

class YesodPersist y where
    type YesodDB y :: (* -> *) -> * -> *
    runDB :: YesodDB y (GHandler sub y) a -> GHandler sub y a
