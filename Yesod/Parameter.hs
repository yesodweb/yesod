{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-} -- Parameter String
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
module Yesod.Parameter
    (
      -- * Parameter
      -- $param_overview
      Parameter (..)
    , ParamType (..)
    , ParamName
    , ParamValue
    , ParamAttempt (..)
    , ParamException
      -- * Exceptions
    , ParameterCountException (..)
    , InvalidBool (..)
    ) where

import Data.Time.Calendar (Day)
import Control.Applicative
import Data.Typeable (Typeable)
import Control.Exception (Exception, SomeException (..))
import Data.Attempt
import qualified Safe.Failure as SF
import Data.Convertible.Text

-- FIXME instead of plain Attempt, an Attempt that defines better error
-- reporting (eg, multilingual)

-- $param_overview
-- In Restful, all of the underlying parameter values are strings. They can
-- come from multiple sources: GET parameters, URL rewriting (FIXME: link),
-- cookies, etc. However, most applications eventually want to convert
-- those strings into something else, like 'Int's. Additionally, it is
-- often desirable to allow multiple values, or no value at all.
--
-- That is what the parameter concept is for. A 'Parameter' is any value
-- which can be converted from a 'String', or list of 'String's.

-- | Where this parameter came from.
data ParamType =
    GetParam
    | PostParam
    | CookieParam
    deriving (Eq, Show)

-- | In GET parameters, the key. In cookies, the cookie name. So on and so
-- forth.
type ParamName = String

-- | The 'String' value of a parameter, such as cookie content.
type ParamValue = String

-- | Anything which can be converted from a list of 'String's.
--
-- The default implementation of 'readParams' will error out if given
-- anything but 1 'ParamValue'. This is usually what you want.
--
-- Minimal complete definition: either 'readParam' or 'readParams'.
class Parameter a where
    -- | Convert a string into the desired value, or explain why that can't
    -- happen.
    readParam :: ParamValue -> Attempt a
    readParam = readParams . return

    -- | Convert a list of strings into the desired value, or explain why
    -- that can't happen.
    readParams :: [ParamValue] -> Attempt a
    readParams [x] = readParam x
    readParams [] = failure MissingParameter
    readParams xs = failure $ ExtraParameters $ length xs

data ParamAttempt v = ParamSuccess v
                    | ParamFailure ParamException
instance Functor ParamAttempt where
    fmap _ (ParamFailure pf) = ParamFailure pf
    fmap f (ParamSuccess v) = ParamSuccess $ f v
instance Applicative ParamAttempt where
    pure = ParamSuccess
    (ParamFailure pf1) <*> (ParamFailure pf2) = ParamFailure $ pf1 ++ pf2
    (ParamFailure pf) <*> _ = ParamFailure pf
    _ <*> ParamFailure pf = ParamFailure pf
    (ParamSuccess f) <*> (ParamSuccess v) = ParamSuccess $ f v
instance Try ParamAttempt where
    type Error ParamAttempt = ParamException
    try (ParamSuccess v) = pure v
    try (ParamFailure f) = failure f
type ParamException = [((ParamType, ParamName, [ParamValue]), SomeException)]

data ParameterCountException = MissingParameter | ExtraParameters Int
    deriving (Show, Typeable)
instance Exception ParameterCountException

instance Parameter a => Parameter (Maybe a) where
    readParams [] = return Nothing
    readParams [x] = Just `fmap` readParam x
    readParams xs = failure $ ExtraParameters $ length xs

instance Parameter a => Parameter [a] where
    readParams = mapM readParam where

instance Parameter String where
    readParam = return

instance Parameter Int where
    readParam = ca

instance Parameter Integer where
    readParam = SF.read

instance Parameter Day where
    readParam = ca

-- for checkboxes; checks for presence or a "false" value
instance Parameter Bool where
    readParams [] = return False
    readParams ["false"] = return False -- FIXME more values?
    readParams [_] = return True
    readParams x = failure $ InvalidBool x

data InvalidBool = InvalidBool [ParamValue]
    deriving (Show, Typeable)
instance Exception InvalidBool
