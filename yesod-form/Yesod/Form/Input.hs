{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Form.Input
    ( FormInput (..)
    , runInputGet
    , runInputPost
    , ireq
    , iopt
    ) where

import Yesod.Form.Types
import Yesod.Form.Fields (FormMessage (MsgInputNotFound))
import Data.Text (Text)
import Control.Applicative (Applicative (..))
import Yesod.Handler (GHandler, GGHandler, invalidArgs, runRequestBody, getRequest, getYesod, liftIOHandler)
import Yesod.Request (reqGetParams, languages)
import Control.Monad (liftM)
import Yesod.Widget (GWidget)
import Yesod.Message (RenderMessage (..))

type DText = [Text] -> [Text]
newtype FormInput sub master a = FormInput { unFormInput :: master -> [Text] -> Env -> GGHandler sub master IO (Either DText a) }
instance Functor (FormInput sub master) where
    fmap a (FormInput f) = FormInput $ \c d e -> fmap (either Left (Right . a)) $ f c d e
instance Applicative (FormInput sub master) where
    pure = FormInput . const . const . const . return . Right
    (FormInput f) <*> (FormInput x) = FormInput $ \c d e -> do
        res1 <- f c d e
        res2 <- x c d e
        return $ case (res1, res2) of
            (Left a, Left b) -> Left $ a . b
            (Left a, _) -> Left a
            (_, Left b) -> Left b
            (Right a, Right b) -> Right $ a b

ireq :: (RenderMessage master msg, RenderMessage master FormMessage) => Field sub master a -> Text -> FormInput sub master a
ireq field name = FormInput $ \m l env -> do
      let filteredEnv = map snd $ filter (\y -> fst y == name) env
      emx <- fieldParse field $ filteredEnv
      return $ case emx of
          Left (SomeMessage e) -> Left $ (:) $ renderMessage m l e
          Right Nothing -> Left $ (:) $ renderMessage m l $ MsgInputNotFound name
          Right (Just a) -> Right a

iopt :: RenderMessage master msg => Field sub master a -> Text -> FormInput sub master (Maybe a)
iopt field name = FormInput $ \m l env -> do
      let filteredEnv = map snd $ filter (\y -> fst y == name) env
      emx <- fieldParse field $ filteredEnv
      return $ case emx of
        Left (SomeMessage e) -> Left $ (:) $ renderMessage m l e
        Right x -> Right x

runInputGet :: FormInput sub master a -> GHandler sub master a
runInputGet (FormInput f) = do
    env <- liftM reqGetParams getRequest
    m <- getYesod
    l <- languages
    emx <- liftIOHandler $ f m l env
    case emx of
        Left errs -> invalidArgs $ errs []
        Right x -> return x

runInputPost :: FormInput sub master a -> GHandler sub master a
runInputPost (FormInput f) = do
    env <- liftM fst runRequestBody
    m <- getYesod
    l <- languages
    emx <- liftIOHandler $ f m l env
    case emx of
        Left errs -> invalidArgs $ errs []
        Right x -> return x
