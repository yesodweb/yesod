-- | The basic typeclass for a Yesod application.
module Yesod.Yesod
    ( Yesod (..)
    , toHackApp
    ) where

import Yesod.Rep
import Data.Object.Html (toHtmlObject)
import Yesod.Response
import Yesod.Request
import Yesod.Constants
import Yesod.Definitions
import Yesod.Resource
import Yesod.Handler

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Convertible.Text
import Web.Encodings
import Control.Arrow ((***))

import qualified Hack
import Hack.Middleware.CleanPath
import Hack.Middleware.ClientSession
import Hack.Middleware.Gzip
import Hack.Middleware.Jsonp
import Hack.Middleware.MethodOverride

type ContentPair = (ContentType, Content)

class Yesod a where
    handlers ::
        [(ResourcePatternString,
          [(Verb, [ContentType] -> Handler a ContentPair)])]

    -- | The encryption key to be used for encrypting client sessions.
    encryptKey :: a -> IO Word256
    encryptKey _ = getKey defaultKeyFile

    -- | Output error response pages.
    errorHandler :: ErrorResult -> [ContentType] -> Handler a ContentPair
    errorHandler = defaultErrorHandler

    -- | Whether or not we should check for overlapping resource names.
    checkOverlaps :: a -> Bool
    checkOverlaps = const True

    -- | An absolute URL to the root of the application.
    approot :: a -> Approot

defaultErrorHandler :: Yesod y
                    => ErrorResult
                    -> [ContentType]
                    -> Handler y ContentPair
defaultErrorHandler NotFound cts = do
    rr <- askRawRequest
    return $ chooseRep (toHtmlObject $ "Not found: " ++ show rr) cts
defaultErrorHandler (Redirect url) cts =
    return $ chooseRep (toHtmlObject $ "Redirect to: " ++ url) cts
defaultErrorHandler PermissionDenied cts =
    return $ chooseRep (toHtmlObject "Permission denied") cts
defaultErrorHandler (InvalidArgs ia) cts =
    return $ chooseRep (toHtmlObject
            [ ("errorMsg", toHtmlObject "Invalid arguments")
            , ("messages", toHtmlObject ia)
            ]) cts
defaultErrorHandler (InternalError e) cts =
    return $ chooseRep (toHtmlObject
                [ ("Internal server error", e)
                ]) cts

-- | For type signature reasons.
handlers' :: Yesod y => y ->
             [(ResourcePatternString,
              [(Verb, [ContentType] -> Handler y ContentPair)])]
handlers' _ = handlers

toHackApp :: Yesod y => y -> Hack.Application
toHackApp a env = do
    let patterns = map fst $ handlers' a
    when (checkOverlaps a) $ checkResourceName patterns -- FIXME maybe this should be done compile-time?
    key <- encryptKey a
    let app' = toHackApp' a
        middleware =
                [ gzip
                , cleanPath
                , jsonp
                , methodOverride
                , clientsession [authCookieName] key
                ]
        app = foldr ($) app' middleware
    app env

toHackApp' :: Yesod y => y -> Hack.Application
toHackApp' y env = do
    let (Right resource) = splitPath $ Hack.pathInfo env
        types = httpAccept env
        (handler, urlParams') = fromMaybe (notFound, []) $ do
            (verbPairs, urlParams'') <- lookupHandlers resource
            let verb = cs $ Hack.requestMethod env
            handler'' <- lookup verb verbPairs
            return (handler'' types, urlParams'')
        rr = envToRawRequest urlParams' env
    runHandler' handler rr y

httpAccept :: Hack.Env -> [ContentType]
httpAccept = undefined

lookupHandlers :: Yesod y
               => Resource
               -> Maybe
                  ( [(Verb, [ContentType] -> Handler y ContentPair)]
                  , [(ParamName, ParamValue)]
                  )
lookupHandlers = undefined

runHandler' :: Yesod y
            => Handler y ContentPair
            -> RawRequest
            -> y
            -> IO Hack.Response
runHandler' = undefined

envToRawRequest :: [(ParamName, ParamValue)] -> Hack.Env -> RawRequest
envToRawRequest urlParams' env =
    let (Right rawPieces) = splitPath $ Hack.pathInfo env
        gets' = decodeUrlPairs $ Hack.queryString env :: [(String, String)]
        clength = fromMaybe "0" $ lookup "Content-Length" $ Hack.http env
        ctype = fromMaybe "" $ lookup "Content-Type" $ Hack.http env
        (posts, files) = map (cs *** cs) *** map (cs *** convertFileInfo)
                       $ parsePost ctype clength
                       $ Hack.hackInput env
        rawCookie = fromMaybe "" $ lookup "Cookie" $ Hack.http env
        cookies' = decodeCookies rawCookie :: [(String, String)]
        langs = ["en"] -- FIXME
     in RawRequest rawPieces urlParams' gets' posts cookies' files env langs

convertFileInfo :: ConvertSuccess a b => FileInfo a c -> FileInfo b c
convertFileInfo (FileInfo a b c) =
    FileInfo (convertSuccess a) (convertSuccess b) c
