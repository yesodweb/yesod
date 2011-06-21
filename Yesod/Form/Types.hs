{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Yesod.Form.Types
    ( -- * Helpers
      Enctype (..)
    , FormResult (..)
    , Env
    , FileEnv
    , Ints (..)
      -- * Form
    , Form
    , AForm (..)
      -- * Build forms
    , Field (..)
    , FieldSettings (..)
    , FieldView (..)
    ) where

import Control.Monad.Trans.RWS (RWST)
import Yesod.Request (FileInfo)
import Data.Text (Text)
import Data.Monoid (Monoid (..))
import Text.Blaze (Html, ToHtml (toHtml))
import Control.Applicative ((<$>), Applicative (..))
import Control.Monad (liftM)
import Data.String (IsString (..))
import Control.Monad.Trans.Class (MonadTrans (..))

-- | A form can produce three different results: there was no data available,
-- the data was invalid, or there was a successful parse.
--
-- The 'Applicative' instance will concatenate the failure messages in two
-- 'FormResult's.
data FormResult a = FormMissing
                  | FormFailure [Text]
                  | FormSuccess a
    deriving Show
instance Functor FormResult where
    fmap _ FormMissing = FormMissing
    fmap _ (FormFailure errs) = FormFailure errs
    fmap f (FormSuccess a) = FormSuccess $ f a
instance Applicative FormResult where
    pure = FormSuccess
    (FormSuccess f) <*> (FormSuccess g) = FormSuccess $ f g
    (FormFailure x) <*> (FormFailure y) = FormFailure $ x ++ y
    (FormFailure x) <*> _ = FormFailure x
    _ <*> (FormFailure y) = FormFailure y
    _ <*> _ = FormMissing
instance Monoid m => Monoid (FormResult m) where
    mempty = pure mempty
    mappend x y = mappend <$> x <*> y

-- | The encoding type required by a form. The 'ToHtml' instance produces values
-- that can be inserted directly into HTML.
data Enctype = UrlEncoded | Multipart
    deriving (Eq, Enum, Bounded)
instance ToHtml Enctype where
    toHtml UrlEncoded = "application/x-www-form-urlencoded"
    toHtml Multipart = "multipart/form-data"
instance Monoid Enctype where
    mempty = UrlEncoded
    mappend UrlEncoded UrlEncoded = UrlEncoded
    mappend _ _ = Multipart

data Ints = IntCons Int Ints | IntSingle Int
instance Show Ints where
    show (IntSingle i) = show i
    show (IntCons i is) = show i ++ ('-' : show is)

type Env = [(Text, Text)] -- FIXME use a Map
type FileEnv = [(Text, FileInfo)]

type Form master m a = RWST (Maybe (Env, FileEnv), master, [Text]) Enctype Ints m a

newtype AForm xml master m a = AForm
    { unAForm :: (master, [Text]) -> Maybe (Env, FileEnv) -> Ints -> m (FormResult a, xml, Ints, Enctype)
    }
instance Monad m => Functor (AForm xml msg m) where
    fmap f (AForm a) =
        AForm $ \x y z -> liftM go $ a x y z
      where
        go (w, x, y, z) = (fmap f w, x, y, z)
instance (Monad m, Monoid xml) => Applicative (AForm xml msg m) where
    pure x = AForm $ const $ const $ \ints -> return (FormSuccess x, mempty, ints, mempty)
    (AForm f) <*> (AForm g) = AForm $ \mr env ints -> do
        (a, b, ints', c) <- f mr env ints
        (x, y, ints'', z) <- g mr env ints'
        return (a <*> x, b `mappend` y, ints'', c `mappend` z)
instance (Monad m, Monoid xml, Monoid a) => Monoid (AForm xml msg m a) where
    mempty = pure mempty
    mappend a b = mappend <$> a <*> b
instance Monoid xml => MonadTrans (AForm xml msg) where
    lift mx = AForm $ const $ const $ \ints -> do
        x <- mx
        return (pure x, mempty, ints, mempty)

data FieldSettings msg = FieldSettings
    { fsLabel :: msg
    , fsTooltip :: Maybe msg
    , fsId :: Maybe Text
    , fsName :: Maybe Text
    }

instance (a ~ Text) => IsString (FieldSettings a) where
    fromString s = FieldSettings (fromString s) Nothing Nothing Nothing

data FieldView xml = FieldView
    { fvLabel :: Html
    , fvTooltip :: Maybe Html
    , fvId :: Text
    , fvInput :: xml
    , fvErrors :: Maybe Html
    , fvRequired :: Bool
    }

data Field xml msg a = Field
    { fieldParse :: [Text] -> Either msg (Maybe a)
    , fieldView :: Text -- ^ ID
                -> Text -- ^ name
                -> Either Text a -- ^ value could be invalid text or a legitimate a
                -> Bool -- ^ required?
                -> xml
    }
