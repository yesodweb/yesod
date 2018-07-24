{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module YesodCoreTest.ErrorHandling
    ( errorHandlingTest
    , Widget
    , resourcesApp
    ) where
import Yesod.Core
import Test.Hspec
import Network.Wai
import Network.Wai.Test
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import Control.Exception (SomeException, try)
import Network.HTTP.Types (Status, mkStatus)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Monoid (mconcat)
import Data.Text (Text, pack)
import Control.Monad (forM_)
import Control.Monad.Trans.State (StateT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified UnliftIO.Exception as E

data App = App

mkYesod "App" [parseRoutes|
/               HomeR GET
/not_found      NotFoundR POST
/first_thing    FirstThingR POST
/after_runRequestBody AfterRunRequestBodyR POST
/error-in-body ErrorInBodyR GET
/error-in-body-noeval ErrorInBodyNoEvalR GET
/override-status OverrideStatusR GET
/error/#Int ErrorR GET

-- https://github.com/yesodweb/yesod/issues/658
/builder BuilderR GET
/file-bad-len FileBadLenR GET
/file-bad-name FileBadNameR GET

/good-builder GoodBuilderR GET

/auth-not-accepted AuthNotAcceptedR GET
/auth-not-adequate AuthNotAdequateR GET
/args-not-valid ArgsNotValidR POST
/only-plain-text OnlyPlainTextR GET
|]

overrideStatus :: Status
overrideStatus = mkStatus 15 "OVERRIDE"

instance Yesod App where
    errorHandler (InvalidArgs ["OVERRIDE"]) = sendResponseStatus overrideStatus ("OH HAI" :: String)
    errorHandler x = defaultErrorHandler x

getHomeR :: Handler Html
getHomeR = do
    $logDebug "Testing logging"
    defaultLayout $ toWidget [hamlet|
$doctype 5

<html>
  <body>
    <form method=post action=@{NotFoundR}>
      <input type=submit value="Not found">
    <form method=post action=@{FirstThingR}>
      <input type=submit value="Error is thrown first thing in handler">
    <form method=post action=@{AfterRunRequestBodyR}>
      <input type=submit value="BUGGY: Error thrown after runRequestBody">
|]

postNotFoundR, postFirstThingR, postAfterRunRequestBodyR :: Handler Html
postNotFoundR = do
   (_, _files) <- runRequestBody
   _ <- notFound
   getHomeR

postFirstThingR = do
   _ <- error "There was an error 3.14159"
   getHomeR

postAfterRunRequestBodyR = do
   x <- runRequestBody
   _ <- error $ show $ fst x
   getHomeR

getErrorInBodyR :: Handler Html
getErrorInBodyR = do
    let foo = error "error in body 19328" :: String
    defaultLayout [whamlet|#{foo}|]

getErrorInBodyNoEvalR :: Handler (DontFullyEvaluate Html)
getErrorInBodyNoEvalR = fmap DontFullyEvaluate getErrorInBodyR

getOverrideStatusR :: Handler ()
getOverrideStatusR = invalidArgs ["OVERRIDE"]

getBuilderR :: Handler TypedContent
getBuilderR = return $ TypedContent "ignored" $ ContentBuilder (error "builder-3.14159") Nothing

getFileBadLenR :: Handler TypedContent
getFileBadLenR = return $ TypedContent "ignored" $ ContentFile "yesod-core.cabal" (error "filebadlen")

getFileBadNameR :: Handler TypedContent
getFileBadNameR = return $ TypedContent "ignored" $ ContentFile (error "filebadname") Nothing

goodBuilderContent :: Builder
goodBuilderContent = Data.Monoid.mconcat $ replicate 100 $ "This is a test\n"

getGoodBuilderR :: Handler TypedContent
getGoodBuilderR = return $ TypedContent "text/plain" $ toContent goodBuilderContent

getErrorR :: Int -> Handler ()
getErrorR 1 = setSession undefined "foo"
getErrorR 2 = setSession "foo" undefined
getErrorR 3 = deleteSession undefined
getErrorR 4 = addHeader undefined "foo"
getErrorR 5 = addHeader "foo" undefined
getErrorR 6 = expiresAt undefined
getErrorR 7 = setLanguage undefined
getErrorR 8 = cacheSeconds undefined
getErrorR 9 = setUltDest (undefined :: Text)
getErrorR 10 = setMessage undefined
getErrorR x = error $ "getErrorR: " ++ show x

getAuthNotAcceptedR :: Handler TypedContent
getAuthNotAcceptedR = notAuthenticated

getAuthNotAdequateR :: Handler TypedContent
getAuthNotAdequateR = permissionDenied "That doesn't belong to you. "

postArgsNotValidR :: Handler TypedContent
postArgsNotValidR = invalidArgs ["Doesn't matter.", "Don't want it."]

getOnlyPlainTextR :: Handler TypedContent
getOnlyPlainTextR = selectRep $ provideRepType "text/plain" $ return ("Only plain text." :: Text)

errorHandlingTest :: Spec
errorHandlingTest = describe "Test.ErrorHandling" $ do
      it "says not found" caseNotFound
      it "says 'There was an error' before runRequestBody" caseBefore
      it "says 'There was an error' after runRequestBody" caseAfter
      it "error in body == 500" caseErrorInBody
      it "error in body, no eval == 200" caseErrorInBodyNoEval
      it "can override status code" caseOverrideStatus
      it "builder" caseBuilder
      it "file with bad len" caseFileBadLen
      it "file with bad name" caseFileBadName
      it "builder includes content-length" caseGoodBuilder
      forM_ [1..10] $ \i -> it ("error case " ++ show i) (caseError i)
      it "accept DVI file, invalid args -> 400" caseDviInvalidArgs
      it "accept audio, not authenticated -> 401" caseAudioNotAuthenticated
      it "accept CSS, permission denied -> 403" caseCssPermissionDenied
      it "accept image, non-existent path -> 404" caseImageNotFound
      it "accept video, bad method -> 405" caseVideoBadMethod

runner :: Session a -> IO a
runner f = toWaiApp App >>= runSession f

caseNotFound :: IO ()
caseNotFound = runner $ do
    res <- request defaultRequest
            { pathInfo = ["not_found"]
            , requestMethod = "POST"
            }
    assertStatus 404 res
    assertBodyContains "Not Found" res

caseBefore :: IO ()
caseBefore = runner $ do
    res <- request defaultRequest
            { pathInfo = ["first_thing"]
            , requestMethod = "POST"
            }
    assertStatus 500 res
    assertBodyContains "There was an error 3.14159" res

caseAfter :: IO ()
caseAfter = runner $ do
    let content = "foo=bar&baz=bin12345"
    res <- srequest SRequest
        { simpleRequest = defaultRequest
            { pathInfo = ["after_runRequestBody"]
            , requestMethod = "POST"
            , requestHeaders =
                [ ("content-type", "application/x-www-form-urlencoded")
                , ("content-length", S8.pack $ show $ L.length content)
                ]
            }
        , simpleRequestBody = content
        }
    assertStatus 500 res
    assertBodyContains "bin12345" res

caseErrorInBody :: IO ()
caseErrorInBody = runner $ do
    res <- request defaultRequest { pathInfo = ["error-in-body"] }
    assertStatus 500 res
    assertBodyContains "error in body 19328" res

caseErrorInBodyNoEval :: IO ()
caseErrorInBodyNoEval = do
    eres <- try $ runner $ do
        request defaultRequest { pathInfo = ["error-in-body-noeval"] }
    case eres of
        Left (_ :: SomeException) -> return ()
        Right x -> error $ "Expected an exception, got: " ++ show x

caseOverrideStatus :: IO ()
caseOverrideStatus = runner $ do
    res <- request defaultRequest { pathInfo = ["override-status"] }
    assertStatus 15 res

caseBuilder :: IO ()
caseBuilder = runner $ do
    res <- request defaultRequest { pathInfo = ["builder"] }
    assertStatus 500 res
    assertBodyContains "builder-3.14159" res

caseFileBadLen :: IO ()
caseFileBadLen = runner $ do
    res <- request defaultRequest { pathInfo = ["file-bad-len"] }
    assertStatus 500 res
    assertBodyContains "filebadlen" res

caseFileBadName :: IO ()
caseFileBadName = runner $ do
    res <- request defaultRequest { pathInfo = ["file-bad-name"] }
    assertStatus 500 res
    assertBodyContains "filebadname" res

caseGoodBuilder :: IO ()
caseGoodBuilder = runner $ do
    res <- request defaultRequest { pathInfo = ["good-builder"] }
    assertStatus 200 res
    let lbs = toLazyByteString goodBuilderContent
    assertBody lbs res
    assertHeader "content-length" (S8.pack $ show $ L.length lbs) res

caseError :: Int -> IO ()
caseError i = runner $ do
    res <- request defaultRequest { pathInfo = ["error", pack $ show i] }
    ReaderT $ \r -> StateT $ \s -> runStateT (runReaderT (assertStatus 500 res) r) s `E.catch` \e -> do
        liftIO $ print res
        E.throwIO (e :: E.SomeException)

caseDviInvalidArgs :: IO ()
caseDviInvalidArgs = runner $ do
    res <- request defaultRequest
            { pathInfo = ["args-not-valid"]
            , requestMethod = "POST"
            , requestHeaders =
                ("accept", "application/x-dvi") : requestHeaders defaultRequest
            }
    assertStatus 400 res

caseAudioNotAuthenticated :: IO ()
caseAudioNotAuthenticated = runner $ do
    res <- request defaultRequest
            { pathInfo = ["auth-not-accepted"]
            , requestHeaders =
                ("accept", "audio/mpeg") : requestHeaders defaultRequest
            }
    assertStatus 401 res

caseCssPermissionDenied :: IO ()
caseCssPermissionDenied = runner $ do
    res <- request defaultRequest
            { pathInfo = ["auth-not-adequate"]
            , requestHeaders =
                ("accept", "text/css") : requestHeaders defaultRequest
            }
    assertStatus 403 res

caseImageNotFound :: IO ()
caseImageNotFound = runner $ do
    res <- request defaultRequest
            { pathInfo = ["not_a_path"]
            , requestHeaders =
                ("accept", "image/jpeg") : requestHeaders defaultRequest
            }
    assertStatus 404 res

caseVideoBadMethod :: IO ()
caseVideoBadMethod = runner $ do
    res <- request defaultRequest
            { pathInfo = ["good-builder"]
            , requestMethod = "DELETE"
            , requestHeaders =
                ("accept", "video/webm") : requestHeaders defaultRequest
            }
    assertStatus 405 res
