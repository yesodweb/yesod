{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Form.Input
    ( FormInput (..)
    , runInputGet
    , runInputPost
    , runInputPostResult
    , ireq
    , iopt
    ) where

import Yesod.Form.Types
import Data.Text (Text)
import Control.Applicative (Applicative (..))
import Yesod.Core
import Control.Monad (liftM)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Arrow ((***))

type DText = [Text] -> [Text]
newtype FormInput m a = FormInput { unFormInput :: HandlerSite m -> [Text] -> Env -> FileEnv -> m (Either DText a) }
instance Monad m => Functor (FormInput m) where
    fmap a (FormInput f) = FormInput $ \c d e e' -> liftM (either Left (Right . a)) $ f c d e e'
instance Monad m => Applicative (FormInput m) where
    pure = FormInput . const . const . const . const . return . Right
    (FormInput f) <*> (FormInput x) = FormInput $ \c d e e' -> do
        res1 <- f c d e e'
        res2 <- x c d e e'
        return $ case (res1, res2) of
            (Left a, Left b) -> Left $ a . b
            (Left a, _) -> Left a
            (_, Left b) -> Left b
            (Right a, Right b) -> Right $ a b

ireq :: (Monad m, RenderMessage (HandlerSite m) FormMessage)
     => Field m a -> Text -> FormInput m a
ireq field name = FormInput $ \m l env fenv -> do
      let filteredEnv = fromMaybe [] $ Map.lookup name env
          filteredFEnv = fromMaybe [] $ Map.lookup name fenv
      emx <- fieldParse field filteredEnv filteredFEnv
      return $ case emx of
          Left (SomeMessage e) -> Left $ (:) $ renderMessage m l e
          Right Nothing -> Left $ (:) $ renderMessage m l $ MsgInputNotFound name
          Right (Just a) -> Right a

iopt :: Monad m => Field m a -> Text -> FormInput m (Maybe a)
iopt field name = FormInput $ \m l env fenv -> do
      let filteredEnv = fromMaybe [] $ Map.lookup name env
          filteredFEnv = fromMaybe [] $ Map.lookup name fenv
      emx <- fieldParse field filteredEnv filteredFEnv
      return $ case emx of
        Left (SomeMessage e) -> Left $ (:) $ renderMessage m l e
        Right x -> Right x

runInputGet :: MonadHandler m => FormInput m a -> m a
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

runInputPost :: MonadHandler m => FormInput m a -> m a
runInputPost fi = do
    emx <- runInputPostHelper fi
    case emx of
        Left errs -> invalidArgs errs
        Right x -> return x

runInputPostResult :: MonadHandler m => FormInput m a -> m (FormResult a)
runInputPostResult fi = do
    emx <- runInputPostHelper fi
    case emx of
        Left errs -> return $ FormFailure errs
        Right x   -> return $ FormSuccess x

runInputPostHelper :: MonadHandler m => FormInput m a -> m (Either [Text] a)
runInputPostHelper (FormInput f) = do
    (env, fenv) <- liftM (toMap *** toMap) runRequestBody
    m <- getYesod
    l <- languages
    fmap (either (Left . ($ [])) Right) $ f m l env fenv
