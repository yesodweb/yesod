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
    ) where

import Control.Monad.Trans.State
import Yesod.Handler
import Data.Monoid (Monoid (..))
import Control.Applicative
import Yesod.Request
import Control.Monad (liftM)

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
instance Show Enctype where
    show UrlEncoded = "application/x-www-form-urlencoded"
    show Multipart = "multipart/form-data"
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
