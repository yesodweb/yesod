{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
-- | Parse forms (and query strings).
module Yesod.Form
    ( Form (..)
    , runFormGeneric
    , runFormPost
    , runFormGet
    , input
    , applyForm
      -- * Specific checks
    , required
    , optional
    , notEmpty
    , checkDay
    , checkBool
    , checkInteger
      -- * Utility
    , catchFormError
    ) where

import Yesod.Request
import Yesod.Response (ErrorResponse)
import Yesod.Handler
import Control.Applicative hiding (optional)
import Data.Time (Day)
import Data.Convertible.Text
import Data.Attempt
import Data.Maybe (fromMaybe)
import "transformers" Control.Monad.IO.Class (MonadIO)
import qualified Safe.Failure

noParamNameError :: String
noParamNameError = "No param name (miscalling of Yesod.Form library)"

data Form x = Form (
                (ParamName -> [ParamValue])
             -> Either [(ParamName, FormError)] (Maybe ParamName, x)
             )

instance Functor Form where
    fmap f (Form x) = Form $ \l -> case x l of
                                    Left errors -> Left errors
                                    Right (pn, x') -> Right (pn, f x')
instance Applicative Form where
    pure x = Form $ \_ -> Right (Nothing, x)
    (Form f') <*> (Form x') = Form $ \l -> case (f' l, x' l) of
        (Right (_, f), Right (_, x)) -> Right (Nothing, f x)
        (Left e1, Left e2) -> Left $ e1 ++ e2
        (Left e, _) -> Left e
        (_, Left e) -> Left e

type FormError = String

runFormGeneric :: MonadFailure ErrorResponse m
               => (ParamName -> [ParamValue]) -> Form x -> m x
runFormGeneric params (Form f) =
    case f params of
        Left es -> invalidArgs es
        Right (_, x) -> return x

-- | Run a form against POST parameters.
runFormPost :: (RequestReader m, MonadFailure ErrorResponse m, MonadIO m)
            => Form x -> m x
runFormPost f = do
    rr <- getRequest
    pp <- postParams rr
    runFormGeneric pp f

-- | Run a form against GET parameters.
runFormGet :: (RequestReader m, MonadFailure ErrorResponse m)
           => Form x -> m x
runFormGet f = do
    rr <- getRequest
    runFormGeneric (getParams rr) f

input :: ParamName -> Form [ParamValue]
input pn = Form $ \l -> Right (Just pn, l pn)

applyForm :: (x -> Either FormError y) -> Form x -> Form y
applyForm f (Form x') =
    Form $ \l ->
        case x' l of
            Left e -> Left e
            Right (pn, x) ->
                case f x of
                    Left e -> Left [(fromMaybe noParamNameError pn, e)]
                    Right y -> Right (pn, y)

required :: Form [ParamValue] -> Form ParamValue
required = applyForm $ \pvs -> case pvs of
                [x] -> Right x
                [] -> Left "No value for required field"
                _ -> Left "Multiple values for required field"

optional :: Form [ParamValue] -> Form (Maybe ParamValue)
optional = applyForm $ \pvs -> case pvs of
                [""] -> Right Nothing
                [x] -> Right $ Just x
                [] -> Right Nothing
                _ -> Left "Multiple values for optional field"

notEmpty :: Form ParamValue -> Form ParamValue
notEmpty = applyForm $ \pv ->
                if null pv
                    then Left "Value required"
                    else Right pv

checkDay :: Form ParamValue -> Form Day
checkDay = applyForm $ attempt (const (Left "Invalid day")) Right . ca

checkBool :: Form [ParamValue] -> Form Bool
checkBool = applyForm $ \pv -> Right $ case pv of
                                        [] -> False
                                        [""] -> False
                                        ["false"] -> False
                                        _ -> True

checkInteger :: Form ParamValue -> Form Integer
checkInteger = applyForm $ \pv ->
    case Safe.Failure.read pv of
        Nothing -> Left "Invalid integer"
        Just i -> Right i

-- | Instead of calling 'failure' with an 'InvalidArgs', return the error
-- messages.
catchFormError :: Form x -> Form (Either [(ParamName, FormError)] x)
catchFormError (Form x) = Form $ \l ->
    case x l of
        Left e -> Right (Nothing, Left e)
        Right (_, v) -> Right (Nothing, Right v)
