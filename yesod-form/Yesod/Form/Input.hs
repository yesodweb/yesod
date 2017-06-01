{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Provides for getting input from either GET or POST params without
-- generating HTML forms. For more information, see:
-- <http://www.yesodweb.com/book/forms#forms_kinds_of_forms>.
module Yesod.Form.Input
    ( FormInput (..)
    , runInputGet
    , runInputGetResult
    , runInputPost
    , runInputPostResult
    , ireq
    , iopt
    ) where

import Yesod.Form.Types
import Data.Text (Text)
import Control.Applicative (Applicative (..))
import Yesod.Core
import Control.Monad (liftM, (<=<))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Arrow ((***))

type DText = [Text] -> [Text]

-- | Type for a form which parses a value of type @a@ with the base monad @m@
-- (usually your @Handler@). Can compose this using its @Applicative@ instance.
newtype FormInput m a = FormInput { unFormInput :: HandlerSite m -> [Text] -> Env -> FileEnv -> m (Either DText a) }
instance Monad m => Functor (FormInput m) where
    fmap a (FormInput f) = FormInput $ \c d e e' -> liftM (either Left (Right . a)) $ f c d e e'
instance Monad m => Control.Applicative.Applicative (FormInput m) where
    pure = FormInput . const . const . const . const . return . Right
    (FormInput f) <*> (FormInput x) = FormInput $ \c d e e' -> do
        res1 <- f c d e e'
        res2 <- x c d e e'
        return $ case (res1, res2) of
            (Left a, Left b) -> Left $ a . b
            (Left a, _) -> Left a
            (_, Left b) -> Left b
            (Right a, Right b) -> Right $ a b

-- | Promote a @Field@ into a @FormInput@, requiring that the value be present
-- and valid.
ireq :: (Monad m, RenderMessage (HandlerSite m) FormMessage)
     => Field m a
     -> Text -- ^ name of the field
     -> FormInput m a
ireq field name = FormInput $ \m l env fenv -> do
      let filteredEnv = fromMaybe [] $ Map.lookup name env
          filteredFEnv = fromMaybe [] $ Map.lookup name fenv
      emx <- fieldParse field filteredEnv filteredFEnv
      return $ case emx of
          Left (SomeMessage e) -> Left $ (:) $ renderMessage m l e
          Right Nothing -> Left $ (:) $ renderMessage m l $ MsgInputNotFound name
          Right (Just a) -> Right a

-- | Promote a @Field@ into a @FormInput@, with its presence being optional. If
-- the value is present but does not parse correctly, the form will still fail.
iopt :: Monad m => Field m a -> Text -> FormInput m (Maybe a)
iopt field name = FormInput $ \m l env fenv -> do
      let filteredEnv = fromMaybe [] $ Map.lookup name env
          filteredFEnv = fromMaybe [] $ Map.lookup name fenv
      emx <- fieldParse field filteredEnv filteredFEnv
      return $ case emx of
        Left (SomeMessage e) -> Left $ (:) $ renderMessage m l e
        Right x -> Right x

-- | Run a @FormInput@ on the GET parameters (i.e., query string). If parsing
-- fails, calls 'invalidArgs'.
runInputGet :: MonadHandler m => FormInput m a -> m a
runInputGet = either invalidArgs return <=< runInputGetHelper

-- | Run a @FormInput@ on the GET parameters (i.e., query string). Does /not/
-- throw exceptions on failure.
--
-- Since 1.4.1
runInputGetResult :: MonadHandler m => FormInput m a -> m (FormResult a)
runInputGetResult = fmap (either FormFailure FormSuccess) . runInputGetHelper

runInputGetHelper :: MonadHandler m => FormInput m a -> m (Either [Text] a)
runInputGetHelper (FormInput f) = do
    env <- liftM (toMap . reqGetParams) getRequest
    m <- getYesod
    l <- languages
    emx <- f m l env Map.empty
    return $ either (Left . ($ [])) Right emx

toMap :: [(Text, a)] -> Map.Map Text [a]
toMap = Map.unionsWith (++) . map (\(x, y) -> Map.singleton x [y])

-- | Run a @FormInput@ on the POST parameters (i.e., request body). If parsing
-- fails, calls 'invalidArgs'.
runInputPost :: MonadHandler m => FormInput m a -> m a
runInputPost = either invalidArgs return <=< runInputPostHelper

-- | Run a @FormInput@ on the POST parameters (i.e., request body). Does /not/
-- throw exceptions on failure.
runInputPostResult :: MonadHandler m => FormInput m a -> m (FormResult a)
runInputPostResult = fmap (either FormFailure FormSuccess) . runInputPostHelper

runInputPostHelper :: MonadHandler m => FormInput m a -> m (Either [Text] a)
runInputPostHelper (FormInput f) = do
    (env, fenv) <- liftM (toMap *** toMap) runRequestBody
    m <- getYesod
    l <- languages
    fmap (either (Left . ($ [])) Right) $ f m l env fenv
