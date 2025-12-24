{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Form.Types
    ( -- * Helpers
      Enctype (..)
    , FormResult (..)
    , FormMessage (..)
    , Env
    , FileEnv
    , Ints (..)
      -- * Form
    , WForm
    , MForm
    , AForm (..)
      -- * Build forms
    , Field (..)
    , FieldSettings (..)
    , FieldView (..)
    , FieldViewFunc
    ) where

import Control.Monad.Trans.RWS (RWST)
import Control.Monad.Trans.Writer (WriterT)
import Data.Text (Text)
import Text.Blaze (Markup, ToMarkup (toMarkup), ToValue (toValue))
#define Html Markup
#define ToHtml ToMarkup
#define toHtml toMarkup
import Control.Applicative (Alternative (..))
import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Data.String (IsString (..))
import Yesod.Core
import qualified Data.Map as Map
import Data.Traversable
import Data.Foldable

-- | A form can produce three different results: there was no data available,
-- the data was invalid, or there was a successful parse.
--
-- The 'Applicative' instance will concatenate the failure messages in two
-- 'FormResult's.
-- The 'Alternative' instance will choose 'FormFailure' before 'FormSuccess',
-- and 'FormMissing' last of all.
data FormResult a = FormMissing
                  | FormFailure [Text]
                  | FormSuccess a
    deriving (Show, Eq)
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
    mappend = (<>)
instance Semigroup m => Semigroup (FormResult m) where
    x <> y = (<>) <$> x <*> y

-- | @since 1.4.5
instance Data.Foldable.Foldable FormResult where
    foldMap f r = case r of
      FormSuccess a -> f a
      FormFailure _errs -> mempty
      FormMissing -> mempty

-- | @since 1.4.5
instance Data.Traversable.Traversable FormResult where
    traverse f r = case r of
      FormSuccess a -> fmap FormSuccess (f a)
      FormFailure errs -> pure (FormFailure errs)
      FormMissing -> pure FormMissing

-- | @since 1.4.15
instance Alternative FormResult where
    empty = FormMissing

    FormFailure e    <|> _             = FormFailure e
    _                <|> FormFailure e = FormFailure e
    FormSuccess s    <|> FormSuccess _ = FormSuccess s
    FormMissing      <|> result        = result
    result           <|> FormMissing   = result

-- | The encoding type required by a form. The 'ToHtml' instance produces values
-- that can be inserted directly into HTML.
data Enctype = UrlEncoded | Multipart
    deriving (Eq, Enum, Bounded)
instance ToHtml Enctype where
    toHtml UrlEncoded = "application/x-www-form-urlencoded"
    toHtml Multipart = "multipart/form-data"
instance ToValue Enctype where
    toValue UrlEncoded = "application/x-www-form-urlencoded"
    toValue Multipart = "multipart/form-data"
instance Monoid Enctype where
    mempty = UrlEncoded
instance Semigroup Enctype where
    UrlEncoded <> UrlEncoded = UrlEncoded
    _          <> _          = Multipart

data Ints = IntCons Int Ints | IntSingle Int
instance Show Ints where
    show (IntSingle i) = show i
    show (IntCons i is) = show i ++ ('-' : show is)

type Env = Map.Map Text [Text]
type FileEnv = Map.Map Text [FileInfo]

-- | 'MForm' variant stacking a 'WriterT'. The following code example using a
-- monadic form 'MForm':
--
-- > formToAForm $ do
-- >   (field1F, field1V) <- mreq textField MsgField1 Nothing
-- >   (field2F, field2V) <- mreq (checkWith field1F textField) MsgField2 Nothing
-- >   (field3F, field3V) <- mreq (checkWith field1F textField) MsgField3 Nothing
-- >   return
-- >     ( MyForm <$> field1F <*> field2F <*> field3F
-- >     , [field1V, field2V, field3V]
-- >     )
--
-- Could be rewritten as follows using 'WForm':
--
-- > wFormToAForm $ do
-- >   field1F <- wreq textField MsgField1 Nothing
-- >   field2F <- wreq (checkWith field1F textField) MsgField2 Nothing
-- >   field3F <- wreq (checkWith field1F textField) MsgField3 Nothing
-- >   return $ MyForm <$> field1F <*> field2F <*> field3F
--
-- @since 1.4.14
type WForm site a = RWST
    (Maybe (Env, FileEnv), site, [Lang])
    Enctype
    Ints
    (WriterT [FieldView site] (HandlerFor site))
    a

type MForm site a = RWST
    (Maybe (Env, FileEnv), site, [Lang])
    Enctype
    Ints
    (HandlerFor site)
    a

newtype AForm site a = AForm
    { unAForm :: (site, [Text])
              -> Maybe (Env, FileEnv)
              -> Ints
              -> HandlerFor site (FormResult a, [FieldView site] -> [FieldView site], Ints, Enctype)
    }
instance Functor (AForm site) where
    fmap f (AForm a) =
        AForm $ \x y z -> liftM go $ a x y z
      where
        go (w, x, y, z) = (fmap f w, x, y, z)
instance Applicative (AForm site) where
    pure x = AForm $ const $ const $ \ints -> return (FormSuccess x, id, ints, mempty)
    (AForm f) <*> (AForm g) = AForm $ \mr env ints -> do
        (a, b, ints', c) <- f mr env ints
        (x, y, ints'', z) <- g mr env ints'
        return (a <*> x, b . y, ints'', c `mappend` z)

#if MIN_VERSION_transformers(0,6,0)
instance Monad (AForm site) where
    (AForm f) >>= k = AForm $ \mr env ints -> do
        (a, b, ints', c) <- f mr env ints
        case a of
          FormSuccess r -> do
            (x, y, ints'', z) <- unAForm (k r) mr env ints'
            return (x, b . y, ints'', c `mappend` z)
          FormFailure err -> pure (FormFailure err, b, ints', c)
          FormMissing -> pure (FormMissing, b, ints', c)
#endif
instance (Monoid a) => Monoid (AForm site a) where
    mempty = pure mempty
    mappend = (<>)
instance (Semigroup a) => Semigroup (AForm site a) where
    a <> b = (<>) <$> a <*> b

-- instance MonadTrans AForm where
--     lift f = AForm $ \_ _ ints -> do
--         x <- f
--         return (FormSuccess x, id, ints, mempty)

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
    , fvInput :: WidgetFor site ()
    , fvErrors :: Maybe Html
    , fvRequired :: Bool
    }

type FieldViewFunc site a
    = Text -- ^ ID
   -> Text -- ^ Name
   -> [(Text, Text)] -- ^ Attributes
   -> Either Text a -- ^ Either (invalid text) or (legitimate result)
   -> Bool -- ^ Required?
   -> WidgetFor site ()

data Field site a = Field
    { fieldParse :: [Text] -> [FileInfo] -> HandlerFor site (Either (SomeMessage site) (Maybe a))
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
                 | MsgInvalidHexColorFormat Text
                 | MsgInvalidDatetimeFormat Text
    deriving (Show, Eq, Read)
