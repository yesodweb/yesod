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
import Yesod.Utils

import Data.Maybe (fromMaybe)
import Data.Convertible.Text
import Web.Encodings
import Control.Arrow ((***))
import Control.Monad (when)

import qualified Hack
import Hack.Middleware.CleanPath
import Hack.Middleware.ClientSession
import Hack.Middleware.Gzip
import Hack.Middleware.Jsonp
import Hack.Middleware.MethodOverride

class Yesod a where
    handlers :: [(ResourcePattern, [(Verb, Handler a RepChooser)])]

    -- | The encryption key to be used for encrypting client sessions.
    encryptKey :: a -> IO Word256
    encryptKey _ = getKey defaultKeyFile

    -- | Output error response pages.
    errorHandler :: ErrorResult -> Handler a RepChooser
    errorHandler = defaultErrorHandler

    -- | Whether or not we should check for overlapping resource names.
    checkOverlaps :: a -> Bool
    checkOverlaps = const True

    -- | An absolute URL to the root of the application.
    approot :: a -> Approot

defaultErrorHandler :: Yesod y
                    => ErrorResult
                    -> Handler y RepChooser
defaultErrorHandler NotFound = do
    rr <- askRawRequest
    return $ chooseRep $ toHtmlObject $ "Not found: " ++ show rr
defaultErrorHandler (Redirect url) =
    return $ chooseRep $ toHtmlObject $ "Redirect to: " ++ url
defaultErrorHandler PermissionDenied =
    return $ chooseRep $ toHtmlObject "Permission denied"
defaultErrorHandler (InvalidArgs ia) =
    return $ chooseRep $ toHtmlObject
            [ ("errorMsg", toHtmlObject "Invalid arguments")
            , ("messages", toHtmlObject ia)
            ]
defaultErrorHandler (InternalError e) =
    return $ chooseRep $ toHtmlObject
                [ ("Internal server error", e)
                ]

-- | For type signature reasons.
handlers' :: Yesod y => y ->
             [(ResourcePattern, [(Verb, Handler y RepChooser)])]
handlers' _ = handlers

toHackApp :: Yesod y => y -> Hack.Application
toHackApp a env = do
    -- FIXME figure out a way to do this check compile-time
    when (checkOverlaps a) $ checkPatterns $ map fst $ handlers' a
    toHackAppUnchecked a env

toHackAppUnchecked :: Yesod y => y -> Hack.Application
toHackAppUnchecked a env = do
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
            return (handler'', urlParams'')
        rr = envToRawRequest urlParams' env
    res <- runHandler handler errorHandler rr y types
    let langs = ["en"] -- FIXME
    responseToHackResponse langs res

httpAccept :: Hack.Env -> [ContentType]
httpAccept = map TypeOther . parseHttpAccept . fromMaybe ""
           . lookup "Accept" . Hack.http

lookupHandlers :: Yesod y
               => Resource
               -> Maybe
                  ( [(Verb, Handler y RepChooser)]
                  , [(ParamName, ParamValue)]
                  )
lookupHandlers r = helper handlers where
    helper [] = Nothing
    helper ((rps, v):rest) =
        case checkPattern (cs rps) r of
            Just up -> Just (v, up)
            Nothing -> helper rest

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
