{-# LANGUAGE OverloadedStrings #-}
-- | Some next-gen helper functions for the scaffolding's configuration system.
module Yesod.Default.Config2
    ( MergedValue (..)
    , applyCurrentEnv
    , getCurrentEnv
    , applyEnv
    ) where

import Data.Monoid
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Text (Text, pack)
import System.Environment (getEnvironment)
import Control.Arrow ((***))
import Control.Applicative ((<$>))
import Control.Monad (guard)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Yaml as Y

newtype MergedValue = MergedValue { getMergedValue :: Value }

instance Monoid MergedValue where
    mempty = MergedValue $ Object H.empty
    MergedValue x `mappend` MergedValue y = MergedValue $ mergeValues x y

-- | Left biased
mergeValues :: Value -> Value -> Value
mergeValues (Object x) (Object y) = Object $ H.unionWith mergeValues x y
mergeValues (Object x) y | H.null x = y
mergeValues x _ = x

applyEnv :: H.HashMap Text Text -> Value -> Value
applyEnv env =
    goV
  where
    goV (Object o) =
        case checkEnv o of
            Just (name, value) ->
                case H.lookup name env of
                    Nothing -> value
                    Just t -> matchType value t
            Nothing -> Object $ goV <$> o
    goV (Array a) = Array (goV <$> a)
    goV v = v

    checkEnv o = do
        guard $ H.size o == 2
        String name <- H.lookup "env" o
        value <- H.lookup "value" o
        return (name, value)

matchType :: Value -> Text -> Value
matchType (Number _) t = tryWrap Number t
matchType (Bool _) t = tryWrap Bool t
matchType _ t = String t

tryWrap :: FromJSON a => (a -> Value) -> Text -> Value
tryWrap con t =
    case Y.decode $ encodeUtf8 t of
        Nothing -> String t
        Just x -> con x

getCurrentEnv :: IO (H.HashMap Text Text)
getCurrentEnv = fmap (H.fromList . map (pack *** pack)) getEnvironment

applyCurrentEnv :: Value -> IO Value
applyCurrentEnv orig = flip applyEnv orig <$> getCurrentEnv
