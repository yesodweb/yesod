{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
-- | The basic typeclass for a Yesod application.
module Yesod.Yesod
    ( -- * Type classes
      Yesod (..)
    , YesodSite (..)
      -- * Convenience functions
    , applyLayout
    , applyLayoutJson
      -- * Defaults
    , defaultErrorHandler
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

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function.
class YesodSite y where
    getSite :: QuasiSite YesodApp y y

-- | Define settings for a Yesod applications. The only required setting is
-- 'approot'; other than that, there are intelligent defaults.
class YesodSite a => Yesod a where
    -- | An absolute URL to the root of the application. Do not include
    -- trailing slash.
    approot :: a -> Approot

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

    -- | Applies some form of layout to <title> and <body> contents of a page.
    defaultLayout :: a
                  -> PageContent (Routes a)
                  -> Request
                  -> Hamlet (Routes a) IO ()
    defaultLayout _ p _ = [$hamlet|
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

-- | Apply the default layout ('defaultLayout') to the given title and body.
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
    content <- hamletToContent $ defaultLayout y pc rr
    return $ RepHtml content

-- | Provide both an HTML and JSON representation for a piece of data, using
-- the default layout for the HTML output ('defaultLayout').
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
    html <- hamletToContent $ defaultLayout y pc rr
    json <- jsonToContent $ toJ x
    return $ RepHtmlJson html json

applyLayout' :: Yesod master
             => String -- ^ title
             -> Hamlet (Routes master) IO () -- ^ body
             -> GHandler sub master ChooseRep
applyLayout' s = fmap chooseRep . applyLayout s

-- | The default error handler for 'errorHandler'.
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
