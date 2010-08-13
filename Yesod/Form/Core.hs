{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.Core
    ( FormResult (..)
    , GForm (..)
    , newFormIdent
    , deeperFormIdent
    , shallowerFormIdent
    , Env
    , FileEnv
    , Enctype (..)
    , Ints (..)
    , requiredFieldHelper
    , optionalFieldHelper
    , fieldsToInput
    , mapFormXml
      -- * Data types
    , FieldInfo (..)
    , FormFieldSettings (..)
    , FieldProfile (..)
      -- * Type synonyms
    , Form
    , Formlet
    , FormField
    , FormletField
    , FormInput
    ) where

import Control.Monad.Trans.State
import Yesod.Handler
import Yesod.Widget
import Data.Monoid (Monoid (..))
import Control.Applicative
import Yesod.Request
import Control.Monad (liftM)
import Text.Hamlet
import Data.String
import Control.Monad (join)

-- | A form can produce three different results: there was no data available,
-- the data was invalid, or there was a successful parse.
--
-- The 'Applicative' instance will concatenate the failure messages in two
-- 'FormResult's.
data FormResult a = FormMissing
                  | FormFailure [String]
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

-- | The encoding type required by a form. The 'Show' instance produces values
-- that can be inserted directly into HTML.
data Enctype = UrlEncoded | Multipart
    deriving (Eq, Enum, Bounded)
instance ToHtml Enctype where
    toHtml UrlEncoded = unsafeByteString "application/x-www-form-urlencoded"
    toHtml Multipart = unsafeByteString "multipart/form-data"
instance Monoid Enctype where
    mempty = UrlEncoded
    mappend UrlEncoded UrlEncoded = UrlEncoded
    mappend _ _ = Multipart

data Ints = IntCons Int Ints | IntSingle Int
instance Show Ints where
    show (IntSingle i) = show i
    show (IntCons i is) = show i ++ '-' : show is

incrInts :: Ints -> Ints
incrInts (IntSingle i) = IntSingle $ i + 1
incrInts (IntCons i is) = (i + 1) `IntCons` is

-- | A generic form, allowing you to specifying the subsite datatype, master
-- site datatype, a datatype for the form XML and the return type.
newtype GForm sub y xml a = GForm
    { deform :: Env -> FileEnv -> StateT Ints (GHandler sub y) (FormResult a, xml, Enctype)
    }

type Env = [(String, String)]
type FileEnv = [(String, FileInfo)]

-- | Get a unique identifier.
newFormIdent :: Monad m => StateT Ints m String
newFormIdent = do
    i <- get
    let i' = incrInts i
    put i'
    return $ 'f' : show i'

deeperFormIdent :: Monad m => StateT Ints m ()
deeperFormIdent = do
    i <- get
    let i' = 1 `IntCons` incrInts i
    put i'

shallowerFormIdent :: Monad m => StateT Ints m ()
shallowerFormIdent = do
    IntCons _ i <- get
    put i

instance Monoid xml => Functor (GForm sub url xml) where
    fmap f (GForm g) =
        GForm $ \env fe -> liftM (first3 $ fmap f) (g env fe)
      where
        first3 f' (x, y, z) = (f' x, y, z)

instance Monoid xml => Applicative (GForm sub url xml) where
    pure a = GForm $ const $ const $ return (pure a, mempty, mempty)
    (GForm f) <*> (GForm g) = GForm $ \env fe -> do
        (f1, f2, f3) <- f env fe
        (g1, g2, g3) <- g env fe
        return (f1 <*> g1, f2 `mappend` g2, f3 `mappend` g3)

-- | Create a required field (ie, one that cannot be blank) from a
-- 'FieldProfile'.ngs
requiredFieldHelper :: FieldProfile sub y a -> FormFieldSettings
                    -> Maybe a -> FormField sub y a
requiredFieldHelper (FieldProfile parse render mkWidget) ffs orig =
  GForm $ \env _ -> do
    let (FormFieldSettings label tooltip theId' name') = ffs
    name <- maybe newFormIdent return name'
    theId <- maybe newFormIdent return theId'
    let (res, val) =
            if null env
                then (FormMissing, maybe "" render orig)
                else case lookup name env of
                        Nothing -> (FormMissing, "")
                        Just "" -> (FormFailure ["Value is required"], "")
                        Just x ->
                            case parse x of
                                Left e -> (FormFailure [e], x)
                                Right y -> (FormSuccess y, x)
    let fi = FieldInfo
            { fiLabel = label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiName = name
            , fiInput = mkWidget theId name val True
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            }
    return (res, [fi], UrlEncoded)

-- | Create an optional field (ie, one that can be blank) from a
-- 'FieldProfile'.
optionalFieldHelper :: FieldProfile sub y a -> FormFieldSettings
                    -> FormletField sub y (Maybe a)
optionalFieldHelper (FieldProfile parse render mkWidget) ffs orig' =
  GForm $ \env _ -> do
    let (FormFieldSettings label tooltip theId' name') = ffs
    let orig = join orig'
    name <- maybe newFormIdent return name'
    theId <- maybe newFormIdent return theId'
    let (res, val) =
            if null env
                then (FormSuccess Nothing, maybe "" render orig)
                else case lookup name env of
                        Nothing -> (FormSuccess Nothing, "")
                        Just "" -> (FormSuccess Nothing, "")
                        Just x ->
                            case parse x of
                                Left e -> (FormFailure [e], x)
                                Right y -> (FormSuccess $ Just y, x)
    let fi = FieldInfo
            { fiLabel = label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiName = name
            , fiInput = mkWidget theId name val False
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            }
    return (res, [fi], UrlEncoded)

fieldsToInput :: [FieldInfo sub y] -> [GWidget sub y ()]
fieldsToInput = map fiInput

-- | Convert the XML in a 'GForm'.
mapFormXml :: (xml1 -> xml2) -> GForm s y xml1 a -> GForm s y xml2 a
mapFormXml f (GForm g) = GForm $ \e fe -> do
    (res, xml, enc) <- g e fe
    return (res, f xml, enc)

-- | Using this as the intermediate XML representation for fields allows us to
-- write generic field functions and then different functions for producing
-- actual HTML. See, for example, 'fieldsToTable' and 'fieldsToPlain'.
data FieldInfo sub y = FieldInfo
    { fiLabel :: Html
    , fiTooltip :: Html
    , fiIdent :: String
    , fiName :: String
    , fiInput :: GWidget sub y ()
    , fiErrors :: Maybe Html
    }

data FormFieldSettings = FormFieldSettings
    { ffsLabel :: Html
    , ffsTooltip :: Html
    , ffsId :: Maybe String
    , ffsName :: Maybe String
    }
instance IsString FormFieldSettings where
    fromString s = FormFieldSettings (string s) mempty Nothing Nothing

-- | A generic definition of a form field that can be used for generating both
-- required and optional fields. See 'requiredFieldHelper and
-- 'optionalFieldHelper'.
data FieldProfile sub y a = FieldProfile
    { fpParse :: String -> Either String a
    , fpRender :: a -> String
    -- | ID, name, value, required
    , fpWidget :: String -> String -> String -> Bool -> GWidget sub y ()
    }

type Form sub y = GForm sub y (GWidget sub y ())
type Formlet sub y a = Maybe a -> Form sub y a
type FormField sub y = GForm sub y [FieldInfo sub y]
type FormletField sub y a = Maybe a -> FormField sub y a
type FormInput sub y = GForm sub y [GWidget sub y ()]
