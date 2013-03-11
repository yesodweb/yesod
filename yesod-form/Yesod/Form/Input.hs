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
import Data.Text (Text)
import Control.Applicative (Applicative (..))
import Yesod.Handler (GHandler, invalidArgs, runRequestBody, getRequest, getYesod)
import Yesod.Core (reqGetParams, languages)
import Control.Monad (liftM)
import Yesod.Core (RenderMessage (..), SomeMessage (..))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Arrow ((***))

type DText = [Text] -> [Text]
newtype FormInput sub master a = FormInput { unFormInput :: master -> [Text] -> Env -> FileEnv -> GHandler sub master (Either DText a) }
instance Functor (FormInput sub master) where
    fmap a (FormInput f) = FormInput $ \c d e e' -> fmap (either Left (Right . a)) $ f c d e e'
instance Applicative (FormInput sub master) where
    pure = FormInput . const . const . const . const . return . Right
    (FormInput f) <*> (FormInput x) = FormInput $ \c d e e' -> do
        res1 <- f c d e e'
        res2 <- x c d e e'
        return $ case (res1, res2) of
            (Left a, Left b) -> Left $ a . b
            (Left a, _) -> Left a
            (_, Left b) -> Left b
            (Right a, Right b) -> Right $ a b

ireq :: (RenderMessage master FormMessage) => Field sub master a -> Text -> FormInput sub master a
ireq field name = FormInput $ \m l env fenv -> do
      let filteredEnv = fromMaybe [] $ Map.lookup name env
          filteredFEnv = fromMaybe [] $ Map.lookup name fenv
      emx <- fieldParse field filteredEnv filteredFEnv
      return $ case emx of
          Left (SomeMessage e) -> Left $ (:) $ renderMessage m l e
          Right Nothing -> Left $ (:) $ renderMessage m l $ MsgInputNotFound name
          Right (Just a) -> Right a

iopt :: Field sub master a -> Text -> FormInput sub master (Maybe a)
iopt field name = FormInput $ \m l env fenv -> do
      let filteredEnv = fromMaybe [] $ Map.lookup name env
          filteredFEnv = fromMaybe [] $ Map.lookup name fenv
      emx <- fieldParse field filteredEnv filteredFEnv
      return $ case emx of
        Left (SomeMessage e) -> Left $ (:) $ renderMessage m l e
        Right x -> Right x

runInputGet :: FormInput sub master a -> GHandler sub master a
runInputGet (FormInput f) = do
    env <- liftM (toMap . reqGetParams) getRequest
    m <- getYesod
    l <- languages
    emx <- f m l env Map.empty
    case emx of
        Left errs -> invalidArgs $ errs []
        Right x -> return x

toMap :: [(Text, a)] -> Map.Map Text [a]
toMap = Map.unionsWith (++) . map (\(x, y) -> Map.singleton x [y])

runInputPost :: FormInput sub master a -> GHandler sub master a
runInputPost (FormInput f) = do
    (env, fenv) <- liftM (toMap *** toMap) runRequestBody
    m <- getYesod
    l <- languages
    emx <- f m l env fenv
    case emx of
        Left errs -> invalidArgs $ errs []
        Right x -> return x
