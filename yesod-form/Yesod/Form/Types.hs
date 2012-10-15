{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Form.Types
    ( -- * Helpers
      Enctype (..)
    , FormResult (..)
    , FormMessage (..)
    , Env
    , FileEnv
    , Ints (..)
      -- * Form
    , MForm
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
import Text.Blaze (Markup, ToMarkup (toMarkup))
#define Html Markup
#define ToHtml ToMarkup
#define toHtml toMarkup
import Control.Applicative ((<$>), Applicative (..))
import Control.Monad (liftM)
import Data.String (IsString (..))
import Yesod.Core (GHandler, GWidget, SomeMessage, MonadLift (..))
import qualified Data.Map as Map

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

type Env = Map.Map Text [Text]
type FileEnv = Map.Map Text FileInfo

type Lang = Text
type MForm sub master a = RWST (Maybe (Env, FileEnv), master, [Lang]) Enctype Ints (GHandler sub master) a

newtype AForm sub master a = AForm
    { unAForm :: (master, [Text]) -> Maybe (Env, FileEnv) -> Ints -> GHandler sub master (FormResult a, [FieldView sub master] -> [FieldView sub master], Ints, Enctype)
    }
instance Functor (AForm sub master) where
    fmap f (AForm a) =
        AForm $ \x y z -> liftM go $ a x y z
      where
        go (w, x, y, z) = (fmap f w, x, y, z)
instance Applicative (AForm sub master) where
    pure x = AForm $ const $ const $ \ints -> return (FormSuccess x, mempty, ints, mempty)
    (AForm f) <*> (AForm g) = AForm $ \mr env ints -> do
        (a, b, ints', c) <- f mr env ints
        (x, y, ints'', z) <- g mr env ints'
        return (a <*> x, b `mappend` y, ints'', c `mappend` z)
instance Monoid a => Monoid (AForm sub master a) where
    mempty = pure mempty
    mappend a b = mappend <$> a <*> b
instance MonadLift (GHandler sub master) (AForm sub master) where
    lift f = AForm $ \_ _ ints -> do
        x <- f
        return (FormSuccess x, id, ints, mempty)

data FieldSettings master = FieldSettings
    { fsLabel :: SomeMessage master
    , fsTooltip :: Maybe (SomeMessage master)
    , fsId :: Maybe Text
    , fsName :: Maybe Text
    , fsAttrs :: [(Text, Text)]
    }

instance IsString (FieldSettings a) where
    fromString s = FieldSettings (fromString s) Nothing Nothing Nothing []

data FieldView sub master = FieldView
    { fvLabel :: Html
    , fvTooltip :: Maybe Html
    , fvId :: Text
    , fvInput :: GWidget sub master ()
    , fvErrors :: Maybe Html
    , fvRequired :: Bool
    }

data Field sub master a = Field
    { fieldParse :: [Text] -> GHandler sub master (Either (SomeMessage master) (Maybe a))
    , fieldView :: Text -- ^ ID
                -> Text -- ^ Name
                -> [(Text, Text)] -- ^ Attributes
                -> Either Text a -- ^ Either (invalid text) or (legitimate result)
                -> Bool -- ^ Required?
                -> GWidget sub master ()
    }

data FormMessage = MsgInvalidInteger Text
                 | MsgInvalidNumber Text
                 | MsgInvalidEntry Text
                 | MsgInvalidUrl Text
                 | MsgInvalidEmail Text
                 | MsgInvalidTimeFormat
                 | MsgInvalidHour Text
                 | MsgInvalidMinute Text
                 | MsgInvalidSecond Text
                 | MsgInvalidDay
                 | MsgCsrfWarning
                 | MsgValueRequired
                 | MsgInputNotFound Text
                 | MsgSelectNone
                 | MsgInvalidBool Text
                 | MsgBoolYes
                 | MsgBoolNo
                 | MsgDelete
    deriving (Show, Eq, Read)
