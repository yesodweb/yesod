{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.Input
    ( FormInput (..)
    , runInputGet
    , runInputPost
    , ireq
    , iopt
    ) where

import Yesod.Form.Types
import Data.Text (Text, append)
import Control.Applicative (Applicative (..))
import Yesod.Handler (GHandler, GGHandler, invalidArgs, runRequestBody, getRequest)
import Yesod.Request (reqGetParams)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import Yesod.Widget (GWidget)

type DText = [Text] -> [Text]
newtype FormInput a = FormInput { unFormInput :: Env -> Either DText a }
instance Functor FormInput where
    fmap a (FormInput f) = FormInput $ \e -> either Left (Right . a) $ f e
instance Applicative FormInput where
    pure = FormInput . const . Right
    (FormInput f) <*> (FormInput x) = FormInput $ \e ->
        case (f e, x e) of
            (Left a, Left b) -> Left $ a . b
            (Left a, _) -> Left a
            (_, Left b) -> Left b
            (Right a, Right b) -> Right $ a b

ireq :: Field (GWidget sub master ()) a -> Text -> FormInput a
ireq field name = FormInput $ \env ->
    case lookup name env of
        Nothing -> Left $ (:) $ append "Input not found: " name -- TRANS
        Just x -> either (Left . (:)) Right $ fieldParse field x

iopt :: Field (GWidget sub master ()) a -> Text -> FormInput (Maybe a)
iopt field name = FormInput $ \env ->
    case fromMaybe "" $ lookup name env of
        "" -> Right Nothing
        x -> either (Left . (:)) (Right . Just) $ fieldParse field x

runInputGet :: Monad monad => FormInput a -> GGHandler sub master monad a
runInputGet (FormInput f) = do
    env <- liftM reqGetParams getRequest
    case f env of
        Left errs -> invalidArgs $ errs []
        Right x -> return x

runInputPost :: FormInput a -> GHandler sub master a
runInputPost (FormInput f) = do
    env <- liftM fst runRequestBody
    case f env of
        Left errs -> invalidArgs $ errs []
        Right x -> return x
