module Import
    ( module Prelude
    , module Foundation
    , (<>)
    , Text
    , module Data.Monoid
    , module Control.Applicative
    ) where

import Prelude hiding (writeFile, readFile)
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
