{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
    , WForm
    , MForm
    , AForm (..)
    , WFormData (..)
    , MFormData (..)
      -- * Build forms
    , Field (..)
    , FieldSettings (..)
    , FieldView (..)
    , FieldViewFunc
    ) where

import RIO
import RIO.Orphans
import Data.Text (Text)
import Data.Monoid (Monoid (..))
import Text.Blaze (Markup, ToMarkup (toMarkup), ToValue (toValue))
#define Html Markup
#define ToHtml ToMarkup
#define toHtml toMarkup
import Control.Applicative ((<$>), Alternative (..), Applicative (..))
import Data.String (IsString (..))
import Yesod.Core
import Yesod.Core.Types
import qualified Data.Map as Map
import Data.Semigroup (Semigroup, (<>))
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
instance Control.Applicative.Applicative FormResult where
    pure = FormSuccess
    (FormSuccess f) <*> (FormSuccess g) = FormSuccess $ f g
    (FormFailure x) <*> (FormFailure y) = FormFailure $ x ++ y
    (FormFailure x) <*> _ = FormFailure x
    _ <*> (FormFailure y) = FormFailure y
    _ <*> _ = FormMissing
instance Data.Monoid.Monoid m => Monoid (FormResult m) where
    mempty = pure mempty
    mappend x y = mappend <$> x <*> y
instance Semigroup m => Semigroup (FormResult m) where
    x <> y = (<>) Control.Applicative.<$> x <*> y

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
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif
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
type WForm site = RIO (WFormData site)
data WFormData site = WFormData
  { wfdViews :: !(IORef ([FieldView site] -> [FieldView site]))
  , wfdMfd :: !(MFormData site)
  }
instance HasHandlerData (WFormData site) where
  type HandlerSite (WFormData site) = site
  type SubHandlerSite (WFormData site) = site
  subHandlerDataL = (lens wfdMfd (\x y -> x { wfdMfd = y })).subHandlerDataL
instance HasResourceMap (WFormData site) where
  resourceMapL = subHandlerDataL.resourceMapL
instance HasLogFunc (WFormData site) where
  logFuncL = subHandlerDataL.logFuncL

type MForm site = RIO (MFormData site)
data MFormData site = MFormData
  { mfdHandlerData :: !(SubHandlerData site site)
  , mfdEnctype :: !(IORef Enctype)
  , mfdParams :: !(Maybe (Env, FileEnv))
  , mfdInts :: !(IORef Ints)
  }
instance HasHandlerData (MFormData site) where
  type HandlerSite (MFormData site) = site
  type SubHandlerSite (MFormData site) = site
  subHandlerDataL = lens mfdHandlerData (\x y -> x { mfdHandlerData = y})
instance HasResourceMap (MFormData site) where
  resourceMapL = subHandlerDataL.resourceMapL
instance HasLogFunc (MFormData site) where
  logFuncL = subHandlerDataL.logFuncL

newtype AForm site a = AForm (WForm site (FormResult a))
  deriving Functor
instance Applicative (AForm site) where
    pure = AForm . pure . pure
    (AForm f) <*> (AForm g) = AForm $ do
        f' <- f
        g' <- g
        pure $ f' <*> g'
instance Monoid a => Monoid (AForm site a) where
    mempty = pure mempty
    mappend a b = mappend <$> a <*> b
instance Semigroup a => Semigroup (AForm site a) where
    a <> b = (<>) <$> a <*> b

data FieldSettings site = FieldSettings
    { fsLabel :: SomeMessage site
    , fsTooltip :: Maybe (SomeMessage site)
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
    deriving (Show, Eq, Read)
