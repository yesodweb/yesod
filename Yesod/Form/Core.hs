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
    , checkForm
    , checkField
    , askParams
    , askFiles
    , liftForm
    , IsForm (..)
    , RunForm (..)
    , GFormMonad
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
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class (lift)
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
newtype GForm s m xml a = GForm
    { deform :: FormInner s m (FormResult a, xml, Enctype)
    }

type GFormMonad s m a = WriterT Enctype (FormInner s m) a

type FormInner s m =
    StateT Ints (
    ReaderT Env (
    ReaderT FileEnv (
    GHandler s m
    )))

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
        GForm $ liftM (first3 $ fmap f) g
      where
        first3 f' (x, y, z) = (f' x, y, z)

instance Monoid xml => Applicative (GForm sub url xml) where
    pure a = GForm $ return (pure a, mempty, mempty)
    (GForm f) <*> (GForm g) = GForm $ do
        (f1, f2, f3) <- f
        (g1, g2, g3) <- g
        return (f1 <*> g1, f2 `mappend` g2, f3 `mappend` g3)

-- | Create a required field (ie, one that cannot be blank) from a
-- 'FieldProfile'.
requiredFieldHelper
    :: IsForm f
    => FieldProfile (FormSub f) (FormMaster f) (FormType f)
    -> FormFieldSettings
    -> Maybe (FormType f)
    -> f
requiredFieldHelper (FieldProfile parse render mkWidget) ffs orig = toForm $ do
    env <- lift ask
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
            { fiLabel = string label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = mkWidget theId name val True
            , fiErrors = case res of
                            FormFailure [x] -> Just $ string x
                            _ -> Nothing
            , fiRequired = True
            }
    let res' = case res of
                FormFailure [e] -> FormFailure [label ++ ": " ++ e]
                _ -> res
    return (res', fi, UrlEncoded)

class IsForm f where
    type FormSub f
    type FormMaster f
    type FormType f
    toForm :: FormInner
                (FormSub f)
                (FormMaster f)
                (FormResult (FormType f),
                 FieldInfo (FormSub f) (FormMaster f),
                 Enctype) -> f
instance IsForm (FormField s m a) where
    type FormSub (FormField s m a) = s
    type FormMaster (FormField s m a) = m
    type FormType (FormField s m a) = a
    toForm x = GForm $ do
        (a, b, c) <- x
        return (a, [b], c)
instance IsForm (GFormMonad s m (FormResult a, FieldInfo s m)) where
    type FormSub (GFormMonad s m (FormResult a, FieldInfo s m)) = s
    type FormMaster (GFormMonad s m (FormResult a, FieldInfo s m)) = m
    type FormType (GFormMonad s m (FormResult a, FieldInfo s m)) = a
    toForm x = do
        (res, fi, enctype) <- lift x
        tell enctype
        return (res, fi)

class RunForm f where
    type RunFormSub f
    type RunFormMaster f
    type RunFormType f
    runFormGeneric :: Env -> FileEnv -> f
                   -> GHandler (RunFormSub f)
                               (RunFormMaster f)
                               (RunFormType f)

instance RunForm (GForm s m xml a) where
    type RunFormSub (GForm s m xml a) = s
    type RunFormMaster (GForm s m xml a) = m
    type RunFormType (GForm s m xml a) =
        (FormResult a, xml, Enctype)
    runFormGeneric env fe (GForm f) =
        runReaderT (runReaderT (evalStateT f $ IntSingle 1) env) fe

instance RunForm (GFormMonad s m a) where
    type RunFormSub (GFormMonad s m a) = s
    type RunFormMaster (GFormMonad s m a) = m
    type RunFormType (GFormMonad s m a) = (a, Enctype)
    runFormGeneric e fe f =
        runReaderT (runReaderT (evalStateT (runWriterT f) $ IntSingle 1) e) fe

-- | Create an optional field (ie, one that can be blank) from a
-- 'FieldProfile'.
optionalFieldHelper
    :: (IsForm f, Maybe b ~ FormType f)
    => FieldProfile (FormSub f) (FormMaster f) b
    -> FormFieldSettings
    -> Maybe (Maybe b)
    -> f
optionalFieldHelper (FieldProfile parse render mkWidget) ffs orig' = toForm $ do
    env <- lift ask
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
            { fiLabel = string label
            , fiTooltip = tooltip
            , fiIdent = theId
            , fiInput = mkWidget theId name val False
            , fiErrors = case res of
                            FormFailure x -> Just $ string $ unlines x
                            _ -> Nothing
            , fiRequired = False
            }
    let res' = case res of
                FormFailure [e] -> FormFailure [label ++ ": " ++ e]
                _ -> res
    return (res', fi, UrlEncoded)

fieldsToInput :: [FieldInfo sub y] -> [GWidget sub y ()]
fieldsToInput = map fiInput

-- | Convert the XML in a 'GForm'.
mapFormXml :: (xml1 -> xml2) -> GForm s y xml1 a -> GForm s y xml2 a
mapFormXml f (GForm g) = GForm $ do
    (res, xml, enc) <- g
    return (res, f xml, enc)

-- | Using this as the intermediate XML representation for fields allows us to
-- write generic field functions and then different functions for producing
-- actual HTML. See, for example, 'fieldsToTable' and 'fieldsToPlain'.
data FieldInfo sub y = FieldInfo
    { fiLabel :: Html
    , fiTooltip :: Html
    , fiIdent :: String
    , fiInput :: GWidget sub y ()
    , fiErrors :: Maybe Html
    , fiRequired :: Bool
    }

data FormFieldSettings = FormFieldSettings
    { ffsLabel :: String
    , ffsTooltip :: Html
    , ffsId :: Maybe String
    , ffsName :: Maybe String
    }
instance IsString FormFieldSettings where
    fromString s = FormFieldSettings s mempty Nothing Nothing

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
checkField :: (a -> Either String b) -> FormField s m a -> FormField s m b
checkField f (GForm form) = GForm $ do
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
                            Nothing -> string err
                            Just x -> x
                    }
    return (res', xml', enc)

askParams :: Monad m => StateT Ints (ReaderT Env m) Env
askParams = lift ask

askFiles :: Monad m => StateT Ints (ReaderT Env (ReaderT FileEnv m)) FileEnv
askFiles = lift $ lift ask

liftForm :: Monad m => m a -> StateT Ints (ReaderT Env (ReaderT FileEnv m)) a
liftForm = lift . lift . lift
