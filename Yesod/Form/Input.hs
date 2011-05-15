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
import Yesod.Handler (GHandler, GGHandler, invalidArgs, runRequestBody, getRequest, getYesod)
import Yesod.Request (reqGetParams, languages)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import Yesod.Widget (GWidget)
import Yesod.Message (RenderMessage (..))

type DText = [Text] -> [Text]
newtype FormInput master a = FormInput { unFormInput :: master -> [Text] -> Env -> Either DText a }
instance Functor (FormInput master) where
    fmap a (FormInput f) = FormInput $ \c d e -> either Left (Right . a) $ f c d e
instance Applicative (FormInput master) where
    pure = FormInput . const . const . const . Right
    (FormInput f) <*> (FormInput x) = FormInput $ \c d e ->
        case (f c d e, x c d e) of
            (Left a, Left b) -> Left $ a . b
            (Left a, _) -> Left a
            (_, Left b) -> Left b
            (Right a, Right b) -> Right $ a b

ireq :: RenderMessage master msg => Field (GWidget sub master ()) msg a -> Text -> FormInput master a
ireq field name = FormInput $ \m l env ->
    case lookup name env of
        Nothing -> Left $ (:) $ append "Input not found: " name -- TRANS
        Just x -> either (Left . (:) . renderMessage m l) Right $ fieldParse field x

iopt :: RenderMessage master msg => Field (GWidget sub master ()) msg a -> Text -> FormInput master (Maybe a)
iopt field name = FormInput $ \m l env ->
    case fromMaybe "" $ lookup name env of
        "" -> Right Nothing
        x -> either (Left . (:) . renderMessage m l) (Right . Just) $ fieldParse field x

runInputGet :: Monad monad => FormInput master a -> GGHandler sub master monad a
runInputGet (FormInput f) = do
    env <- liftM reqGetParams getRequest
    m <- getYesod
    l <- languages
    case f m l env of
        Left errs -> invalidArgs $ errs []
        Right x -> return x

runInputPost :: FormInput master a -> GHandler sub master a
runInputPost (FormInput f) = do
    env <- liftM fst runRequestBody
    m <- getYesod
    l <- languages
    case f m l env of
        Left errs -> invalidArgs $ errs []
        Right x -> return x
