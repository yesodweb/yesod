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
    , FieldViewFunc
    ) where

import Control.Monad.Trans.RWS (RWST)
import Yesod.Core (FileInfo)
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
type FileEnv = Map.Map Text [FileInfo]

type Lang = Text
type MForm site a = RWST (Maybe (Env, FileEnv), site, [Lang]) Enctype Ints (GHandler site) a

newtype AForm site a = AForm
    { unAForm :: (site, [Text]) -> Maybe (Env, FileEnv) -> Ints -> GHandler site (FormResult a, [FieldView site] -> [FieldView site], Ints, Enctype)
    }
instance Functor (AForm site) where
    fmap f (AForm a) =
        AForm $ \x y z -> liftM go $ a x y z
      where
        go (w, x, y, z) = (fmap f w, x, y, z)
instance Applicative (AForm site) where
    pure x = AForm $ const $ const $ \ints -> return (FormSuccess x, mempty, ints, mempty)
    (AForm f) <*> (AForm g) = AForm $ \mr env ints -> do
        (a, b, ints', c) <- f mr env ints
        (x, y, ints'', z) <- g mr env ints'
        return (a <*> x, b `mappend` y, ints'', c `mappend` z)
instance Monoid a => Monoid (AForm site a) where
    mempty = pure mempty
    mappend a b = mappend <$> a <*> b
instance MonadLift (GHandler site) (AForm site) where
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

data FieldView site = FieldView
    { fvLabel :: Html
    , fvTooltip :: Maybe Html
    , fvId :: Text
    , fvInput :: GWidget site ()
    , fvErrors :: Maybe Html
    , fvRequired :: Bool
    }

type FieldViewFunc site a
    = Text -- ^ ID
   -> Text -- ^ Name
   -> [(Text, Text)] -- ^ Attributes
   -> Either Text a -- ^ Either (invalid text) or (legitimate result)
   -> Bool -- ^ Required?
   -> GWidget site ()

data Field site a = Field
    { fieldParse :: [Text] -> [FileInfo] -> GHandler site (Either (SomeMessage site) (Maybe a))
    , fieldView :: FieldViewFunc site a
    , fieldEnctype :: Enctype
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
