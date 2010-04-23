{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
-- | The basic typeclass for a Yesod application.
module Yesod.Yesod
    ( Yesod (..)
    , YesodSite (..)
    , applyLayout
    , applyLayoutJson
    ) where

import Yesod.Content
import Yesod.Request
import Yesod.Hamlet
import Yesod.Handler
import Data.Convertible.Text
import Control.Arrow ((***))
import Network.Wai.Middleware.ClientSession
import qualified Network.Wai as W
import Yesod.Definitions
import Yesod.Json
import Yesod.Internal

import Web.Routes.Quasi (QuasiSite (..))

class YesodSite y where
    getSite :: QuasiSite YesodApp y y

class YesodSite a => Yesod a where
    -- | The encryption key to be used for encrypting client sessions.
    encryptKey :: a -> IO Word256
    encryptKey _ = getKey defaultKeyFile

    -- | Number of minutes before a client session times out. Defaults to
    -- 120 (2 hours).
    clientSessionDuration :: a -> Int
    clientSessionDuration = const 120

    -- | Output error response pages.
    errorHandler :: Yesod y
                 => a
                 -> ErrorResponse
                 -> Handler y ChooseRep
    errorHandler _ = defaultErrorHandler

    -- | Applies some form of layout to <title> and <body> contents of a page. FIXME: use a Maybe here to allow subsites to simply inherit.
    rawApplyLayout :: a
                   -> PageContent (Routes a)
                   -> Request
                   -> Hamlet (Routes a) IO ()
    rawApplyLayout _ p _ = [$hamlet|
!!!
%html
    %head
        %title $pageTitle$
        ^pageHead^
    %body
        ^pageBody^
|] p

    -- | Gets called at the beginning of each request. Useful for logging.
    onRequest :: a -> Request -> IO ()
    onRequest _ _ = return ()

    -- | An absolute URL to the root of the application. Do not include
    -- trailing slash.
    approot :: a -> Approot

-- | A convenience wrapper around 'simpleApplyLayout for HTML-only data.
applyLayout :: Yesod master
            => String -- ^ title
            -> Hamlet (Routes master) IO () -- ^ body
            -> GHandler sub master RepHtml
applyLayout t b = do
    let pc = PageContent
                { pageTitle = cs t
                , pageHead = return ()
                , pageBody = b
                }
    y <- getYesodMaster
    rr <- getRequest
    content <- hamletToContent $ rawApplyLayout y pc rr
    return $ RepHtml content

applyLayoutJson :: Yesod master
                => String -- ^ title
                -> x
                -> (x -> Hamlet (Routes master) IO ())
                -> (x -> Json (Routes master) ())
                -> GHandler sub master RepHtmlJson
applyLayoutJson t x toH toJ = do
    let pc = PageContent
                { pageTitle = cs t
                , pageHead = return () -- FIXME allow user to supply?
                , pageBody = toH x
                }
    y <- getYesodMaster
    rr <- getRequest
    html <- hamletToContent $ rawApplyLayout y pc rr
    json <- jsonToContent $ toJ x
    return $ RepHtmlJson html json

applyLayout' :: Yesod master
             => String -- ^ title
             -> Hamlet (Routes master) IO () -- ^ body
             -> GHandler sub master ChooseRep
applyLayout' s = fmap chooseRep . applyLayout s

defaultErrorHandler :: Yesod y
                    => ErrorResponse
                    -> Handler y ChooseRep
defaultErrorHandler NotFound = do
    r <- waiRequest
    applyLayout' "Not Found" $ [$hamlet|
%h1 Not Found
%p $helper$
|] r
  where
    helper = Unencoded . cs . W.pathInfo
defaultErrorHandler PermissionDenied =
    applyLayout' "Permission Denied" $ [$hamlet|
%h1 Permission denied|] ()
defaultErrorHandler (InvalidArgs ia) =
    applyLayout' "Invalid Arguments" $ [$hamlet|
%h1 Invalid Arguments
%dl
    $forall ias pair
        %dt $pair.fst$
        %dd $pair.snd$
|] ()
  where
    ias _ = map (cs *** cs) ia
defaultErrorHandler (InternalError e) =
    applyLayout' "Internal Server Error" $ [$hamlet|
%h1 Internal Server Error
%p $cs$
|] e
defaultErrorHandler (BadMethod m) =
    applyLayout' "Bad Method" $ [$hamlet|
%h1 Method Not Supported
%p Method "$cs$" not supported
|] m
