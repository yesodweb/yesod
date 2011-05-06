{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Users of the forms library should not need to use this module in general.
-- It is intended only for writing custom forms and form fields.
module Yesod.Form.Core
    ( FormResult (..)
    , GForm (..)
    , newFormIdent
    {- FIXME
    , deeperFormIdent
    , shallowerFormIdent
    -}
    , Env
    , FileEnv
    , Enctype (..)
    , Ints (..)
    , requiredFieldHelper
    , optionalFieldHelper
    , mapFormXml
    {- FIXME
    , checkForm
    , checkField
    -}
    , askParams
    , askFiles
      -- * Data types
    , FieldInfo (..)
    , FormFieldSettings (..)
    , FieldProfile (..)
      -- * Type synonyms
    {- FIXME
    , Form
    , Formlet
    , FormField
    , FormletField
    , FormInput
    -}
    ) where

import Control.Monad.Trans.RWS
import Control.Monad.Trans.Class (lift)
import Yesod.Handler
import Yesod.Widget
import Data.Monoid (Monoid (..))
import Control.Applicative
import Yesod.Request
import Control.Monad (liftM)
import Text.Hamlet
import Text.Blaze (ToHtml (..))
import Data.String
import Control.Monad (join)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Prelude hiding ((++))

(++) :: Monoid a => a -> a -> a
(++) = mappend

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
    toHtml UrlEncoded = unsafeByteString "application/x-www-form-urlencoded"
    toHtml Multipart = unsafeByteString "multipart/form-data"
instance Monoid Enctype where
    mempty = UrlEncoded
    mappend UrlEncoded UrlEncoded = UrlEncoded
    mappend _ _ = Multipart

data Ints = IntCons Int Ints | IntSingle Int
instance Show Ints where
    show (IntSingle i) = show i
    show (IntCons i is) = show i ++ ('-' : show is)

incrInts :: Ints -> Ints
incrInts (IntSingle i) = IntSingle $ i + 1
incrInts (IntCons i is) = (i + 1) `IntCons` is

type GForm xml m a = RWST (Env, FileEnv) (Enctype, xml) Ints m a -- FIXME rename to Form
type Env = [(Text, Text)]
type FileEnv = [(Text, FileInfo)]

-- | Get a unique identifier.
newFormIdent :: (Monoid xml, Monad m) => GForm xml m Text
newFormIdent = do
    i <- get
    let i' = incrInts i
    put i'
    return $ pack $ 'f' : show i'

{- FIXME
deeperFormIdent :: Monad m => StateT Ints m ()
deeperFormIdent = do
    i <- get
    let i' = 1 `IntCons` incrInts i
    put i'

shallowerFormIdent :: Monad m => StateT Ints m ()
shallowerFormIdent = do
    IntCons _ i <- get
    put i
-}

-- | Create a required field (ie, one that cannot be blank) from a
-- 'FieldProfile'.
requiredFieldHelper
    :: (Monoid xml', Monad m)
    => FieldProfile xml a
    -> FormFieldSettings
    -> Maybe a
    -> GForm xml' m (FormResult a, FieldInfo xml)
requiredFieldHelper (FieldProfile parse render mkWidget) ffs orig = do
    env <- askParams
    let (FormFieldSettings label tooltip theId' name') = ffs
    name <- maybe newFormIdent return name'
    theId <- maybe newFormIdent return theId'
    let (res, val) =
            if null env
                then (FormMissing, maybe "" render orig)
                else case lookup name env of
                        Nothing -> (FormMissing, "")
                        Just "" -> (FormFailure ["Value is required"], "") -- TRANS
                        Just x ->
                            case parse x of
                                Left e -> (FormFailure [e], x)
                                Right y -> (FormSuccess y, x)
    let fi = FieldInfo
            { fiLabel = toHtml label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = mkWidget theId name val True
            , fiErrors = case res of
                            FormFailure [x] -> Just $ toHtml x
                            _ -> Nothing
            , fiRequired = True
            }
    let res' = case res of
                FormFailure [e] -> FormFailure [label ++ ": " ++ e]
                _ -> res
    return (res', fi)

-- | Create an optional field (ie, one that can be blank) from a
-- 'FieldProfile'.
optionalFieldHelper
    :: (Monad m, Monoid xml')
    => FieldProfile xml b
    -> FormFieldSettings
    -> Maybe (Maybe b)
    -> GForm xml' m (FormResult (Maybe b), FieldInfo xml)
optionalFieldHelper (FieldProfile parse render mkWidget) ffs orig' = do
    env <- askParams
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
            { fiLabel = toHtml label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = mkWidget theId name val False
            , fiErrors = case res of
                            FormFailure x -> Just $ toHtml $ T.unlines x
                            _ -> Nothing
            , fiRequired = False
            }
    let res' = case res of
                FormFailure [e] -> FormFailure [label ++ ": " ++ e]
                _ -> res
    return (res', fi)

-- | Convert the XML in a 'GForm'.
mapFormXml :: Monad m => (xml1 -> xml2) -> GForm xml1 m a -> GForm xml2 m a
mapFormXml f = mapRWST $ \x -> do
    (a, b, (c, d)) <- x
    return (a, b, (c, f d))

-- | Using this as the intermediate XML representation for fields allows us to
-- write generic field functions and then different functions for producing
-- actual HTML. See, for example, 'fieldsToTable' and 'fieldsToPlain'.
data FieldInfo xml = FieldInfo
    { fiLabel :: Html
    , fiTooltip :: Html
    , fiIdent :: Text
    , fiInput :: xml
    , fiErrors :: Maybe Html
    , fiRequired :: Bool
    }

data FormFieldSettings = FormFieldSettings
    { ffsLabel :: Text
    , ffsTooltip :: Html
    , ffsId :: Maybe Text
    , ffsName :: Maybe Text
    }
instance IsString FormFieldSettings where
    fromString s = FormFieldSettings (pack s) mempty Nothing Nothing

-- | A generic definition of a form field that can be used for generating both
-- required and optional fields. See 'requiredFieldHelper and
-- 'optionalFieldHelper'.
data FieldProfile xml a = FieldProfile
    { fpParse :: Text -> Either Text a
    , fpRender :: a -> Text
    -- | ID, name, value, required
    , fpWidget :: Text -> Text -> Text -> Bool -> xml
    }

{- FIXME
type Form sub y = GForm sub y (GWidget sub y ())
type Formlet sub y a = Maybe a -> Form sub y a
type FormInput sub y = GForm sub y [GWidget sub y ()]
type FormField xml m = GForm xml m [FieldInfo xml]
type FormletField xml m a = Maybe a -> FormField xml a
-}

{- FIXME
-- | Add a validation check to a form.
--
-- Note that if there is a validation error, this message will /not/
-- automatically appear on the form; for that, you need to use 'checkField'.
checkForm :: (a -> FormResult b) -> GForm s m x a -> GForm s m x b
checkForm f (GForm form) = GForm $ do
    (res, xml, enc) <- form
    let res' = case res of
                    FormSuccess a -> f a
                    FormFailure e -> FormFailure e
                    FormMissing -> FormMissing
    return (res', xml, enc)

-- | Add a validation check to a 'FormField'.
--
-- Unlike 'checkForm', the validation error will appear in the generated HTML
-- of the form.
checkField :: (a -> Either Text b) -> FormField s m a -> FormField s m b
checkField f form = do
    (res, xml, enc) <- form
    let (res', merr) =
            case res of
                FormSuccess a ->
                    case f a of
                        Left e -> (FormFailure [e], Just e)
                        Right x -> (FormSuccess x, Nothing)
                FormFailure e -> (FormFailure e, Nothing)
                FormMissing -> (FormMissing, Nothing)
    let xml' =
            case merr of
                Nothing -> xml
                Just err -> flip map xml $ \fi -> fi
                    { fiErrors = Just $
                        case fiErrors fi of
                            Nothing -> toHtml err
                            Just x -> x
                    }
    return (res', xml', enc)
-}

askParams :: (Monoid xml, Monad m) => GForm xml m Env
askParams = liftM fst ask

askFiles :: (Monoid xml, Monad m) => GForm xml m FileEnv
askFiles = liftM snd ask
