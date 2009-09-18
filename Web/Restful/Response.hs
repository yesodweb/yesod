{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
---------------------------------------------------------
--
-- Module        : Web.Restful.Response
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Generating responses.
--
---------------------------------------------------------
module Web.Restful.Response
    ( formatW3
    , HasReps (..)
    , notFound
    , wrapResponse
    , ResponseIO
    , ResponseT
    , Response
    , runResponse
    , deleteCookie
    , redirect
    , addCookie
    , header
    , GenResponse (..)
    , liftIO
    , ErrorResult (..)
    , HasRepsW (..)
    ) where

import Data.ByteString.Class
import Data.Time.Format
import Data.Time.Clock
import System.Locale
import Data.Object
import qualified Data.ByteString.Lazy as B
import Data.Object.Instances
import Data.Maybe (fromJust)

import Control.Monad.Trans

import qualified Hack

type ContentType = String

-- | Something which can be represented as multiple content types.
-- Each content type is called a representation of the data.
class HasReps a where
    -- | Provide an ordered list of possible representations, depending on
    -- content type. If the user asked for a specific response type (like
    -- text/html), then that will get priority. If not, then the first
    -- element in this list will be used.
    reps :: a -> [(ContentType, B.ByteString)]

-- | Wrap up any instance of 'HasReps'.
data HasRepsW = forall a. HasReps a => HasRepsW a

instance HasReps HasRepsW where
    reps (HasRepsW r) = reps r

data ErrorResult =
    Redirect String
    | NotFound
    | InternalError String

getStatus :: ErrorResult -> Int
getStatus (Redirect _) = 303
getStatus NotFound = 404
getStatus (InternalError _) = 500

getHeaders :: ErrorResult -> [Header]
getHeaders (Redirect s) = [Header "Location" s]
getHeaders _ = []

newtype ResponseT m a = ResponseT (m (Either ErrorResult a, [Header]))
type ResponseIO = ResponseT IO
type Response = ResponseIO HasRepsW

runResponse :: (ErrorResult -> HasRepsW)
            -> (ContentType -> B.ByteString -> IO B.ByteString)
            -> [ContentType]
            -> Response
            -> IO Hack.Response
runResponse eh wrapper ctypesAll (ResponseT inside) = do
    (x, headers') <- inside
    let extraHeaders =
            case x of
                Left r -> getHeaders r
                Right _ -> []
    headers <- mapM toPair (headers' ++ extraHeaders)
    let outReps = either (reps . eh) reps x
    let statusCode =
            case x of
                Left r -> getStatus r
                Right _ -> 200
    (ctype, selectedRep) <- chooseRep outReps ctypesAll
    finalRep <- wrapper ctype selectedRep
    let headers'' = ("Content-Type", ctype) : headers
    return $! Hack.Response statusCode headers'' finalRep

chooseRep :: Monad m
          => [(ContentType, B.ByteString)]
          -> [ContentType]
          -> m (ContentType, B.ByteString)
chooseRep rs cs
  | length rs == 0 = fail "All reps must have at least one value"
  | otherwise = do
    let availCs = map fst rs
    case filter (`elem` availCs) cs of
        [] -> return $ head rs
        [ctype] -> return (ctype, fromJust $ lookup ctype rs)
        _ -> fail "Overlapping representations"

toPair :: Header -> IO (String, String)
toPair (AddCookie minutes key value) = do
    now <- getCurrentTime
    let expires = addUTCTime (fromIntegral $ minutes * 60) now
    return ("Set-Cookie", key ++ "=" ++ value ++"; path=/; expires="
                              ++ formatW3 expires)
toPair (DeleteCookie key) = return
    ("Set-Cookie",
     key ++ "=; path=/; expires=Thu, 01-Jan-1970 00:00:00 GMT")
toPair (Header key value) = return (key, value)

wrapResponse :: (Monad m, HasReps rep)
             => ResponseT m rep
             -> ResponseT m HasRepsW
wrapResponse = fmap HasRepsW

instance MonadTrans ResponseT where
    lift ma = ResponseT $ do
        a <- ma
        return (Right a, [])

instance MonadIO ResponseIO where
    liftIO = lift

redirect :: Monad m => String -> ResponseT m a
redirect s = ResponseT (return (Left $ Redirect s, []))

notFound :: Monad m => ResponseT m a
notFound = ResponseT (return (Left NotFound, []))

instance Monad m => Functor (ResponseT m) where
    fmap f x = x >>= return . f

instance Monad m => Monad (ResponseT m) where
    return = lift . return
    fail s = ResponseT (return (Left $ InternalError s, []))
    (ResponseT mx) >>= f = ResponseT $ do
        (x, hs1) <- mx
        case x of
            Left x' -> return (Left x', hs1)
            Right a -> do
                let (ResponseT b') = f a
                (b, hs2) <- b'
                return (b, hs1 ++ hs2)

-- | Headers to be added to a 'Result'.
data Header =
    AddCookie Int String String
    | DeleteCookie String
    | Header String String

addCookie :: Monad m => Int -> String -> String -> ResponseT m ()
addCookie a b c = addHeader $ AddCookie a b c

deleteCookie :: Monad m => String -> ResponseT m ()
deleteCookie = addHeader . DeleteCookie

header :: Monad m => String -> String -> ResponseT m ()
header a b = addHeader $ Header a b

addHeader :: Monad m => Header -> ResponseT m ()
addHeader h = ResponseT (return (Right (), [h]))

instance HasReps () where
    reps _ = [("text/plain", toLazyByteString "")]

data GenResponse = HtmlResponse String
                 | ObjectResponse Object
                 | HtmlOrObjectResponse String Object
instance HasReps GenResponse where
    reps (HtmlResponse h) = [("text/html", toLazyByteString h)]
    reps (ObjectResponse t) = reps t
    reps (HtmlOrObjectResponse h t) =
        ("text/html", toLazyByteString h) : reps t

instance HasReps Object where
    reps o =
        [ ("text/html", unHtml $ safeFromObject o)
        , ("application/json", unJson $ safeFromObject o)
        , ("text/yaml", unYaml $ safeFromObject o)
        ]

instance HasReps [(ContentType, B.ByteString)] where
    reps = id

-- FIXME put in a separate module (maybe Web.Encodings)
formatW3 :: UTCTime -> String
formatW3 = formatTime defaultTimeLocale "%FT%X-08:00"
