{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Integration tests for yesod-form's 'runFormPRG'.
--
-- These live in yesod-test rather than yesod-form: a yesod-test
-- dependency in yesod-form's test suite creates a package-level
-- dependency cycle that Stackage's build planner rejects, see
-- <https://github.com/yesodweb/yesod/issues/1928>.
module PRGSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wai.Test (simpleBody)
import Test.Hspec (Spec)
import qualified Web.ClientSession as CS
import Yesod.Core
import Yesod.Form
import Yesod.Test

data App = App

mkYesod "App" [parseRoutes|
/form FormR GET POST
/two TwoR GET POST
/corrupt CorruptR GET
/stale StaleR GET
|]

instance Yesod App where
    -- A random key per test run, so no client_session_key.aes file is
    -- written into the repository.
    makeSessionBackend _ = do
        key <- snd `fmap` CS.randomKey
        (getCachedDate, _closeDateCacher) <- clientSessionDateCacher (120 * 60)
        return $ Just $ clientSessionBackend key getCachedDate

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

fs :: Text -> FieldSettings App
fs name = FieldSettings (SomeMessage name) Nothing Nothing (Just name) []

personForm :: Html -> MForm Handler (FormResult (Text, Int), Widget)
personForm csrf = do
    (nameRes, nameView) <- mreq textField (fs "name") Nothing
    (ageRes, ageView) <- mreq ageField (fs "age") Nothing
    let widget = [whamlet|
            #{csrf}
            ^{fvInput nameView}
            $maybe err <- fvErrors nameView
                <p .error>#{err}
            ^{fvInput ageView}
            $maybe err <- fvErrors ageView
                <p .error>#{err}
            |]
    return ((,) <$> nameRes <*> ageRes, widget)
  where
    ageField = checkBool (>= (18 :: Int)) ("Too young" :: Text) intField

-- | A one-field form for the two-forms-on-a-page tests; submitting
-- non-numeric input fails with the input preserved.
numForm :: Text -> Html -> MForm Handler (FormResult Int, Widget)
numForm name csrf = do
    (res, view) <- mreq intField (fs name) Nothing
    let widget = [whamlet|
            #{csrf}
            ^{fvInput view}
            $maybe err <- fvErrors view
                <p .error>#{name}: #{err}
            |]
    return (res, widget)

getFormR :: Handler Html
getFormR = do
    mmsg <- getMessage
    ((_res, widget), enctype) <- runFormPRG personForm
    defaultLayout [whamlet|
        $maybe msg <- mmsg
            <p .message>#{msg}
        <form method=post action=@{FormR} enctype=#{enctype}>
            ^{widget}
            <button>go
    |]

postFormR :: Handler Html
postFormR = do
    ((res, _widget), _enctype) <- runFormPRG personForm
    case res of
        FormSuccess _ -> setMessage "registered" >> redirect FormR
        _ -> redirect FormR

getTwoR :: Handler Html
getTwoR = do
    mmsg <- getMessage
    ((_, fooW), fooE) <- runFormPRG $ identifyForm "foo" $ numForm "foonum"
    ((_, barW), barE) <- runFormPRG $ identifyForm "bar" $ numForm "barnum"
    defaultLayout [whamlet|
        $maybe msg <- mmsg
            <p .message>#{msg}
        <form method=post action=@{TwoR} enctype=#{fooE}>
            ^{fooW}
            <button>foo
        <form method=post action=@{TwoR} enctype=#{barE}>
            ^{barW}
            <button>bar
    |]

postTwoR :: Handler Html
postTwoR = do
    ((fooRes, _), _) <- runFormPRG $ identifyForm "foo" $ numForm "foonum"
    ((barRes, _), _) <- runFormPRG $ identifyForm "bar" $ numForm "barnum"
    case (fooRes, barRes) of
        (FormSuccess _, _) -> setMessage "foo ok" >> redirect TwoR
        (_, FormSuccess _) -> setMessage "bar ok" >> redirect TwoR
        _ -> redirect TwoR

-- | Plant garbage bytes where the stash for \/form lives.
getCorruptR :: Handler ()
getCorruptR = setSessionBS "_PRG:/form" "definitely not json"

-- | Plant a decodable stash for \/form, as if a submission with
-- name=stale had failed earlier.
getStaleR :: Handler ()
getStaleR = setSessionBS "_PRG:/form" "{\"name\":[\"stale\"],\"age\":[\"12\"]}"

-- | The CSRF token stays constant for a session, so grab it once from the
-- current response body and reuse it across requests. This avoids
-- 'addToken', which can only read the token from the /previous/ response.
extractToken :: YesodExample App Text
extractToken = do
    mres <- getResponse
    let body = maybe "" (TE.decodeUtf8 . BL.toStrict . simpleBody) mres
        (_, rest) = T.breakOn "name=\"_token\" value=\"" body
    case T.splitOn "\"" (T.drop (T.length "name=\"_token\" value=\"") rest) of
        (token:_) | not (T.null token) -> return token
        _ -> error "no _token in response body"

postParams :: Route App -> Text -> [(Text, Text)] -> YesodExample App ()
postParams route token params = request $ do
    setMethod "POST"
    setUrl route
    addPostParam "_token" token
    mapM_ (uncurry addPostParam) params

spec :: Spec
spec = yesodSpec App $
    ydescribe "runFormPRG" $ do
        yit "renders a blank form when nothing is stashed" $ do
            get FormR
            statusIs 200
            bodyContains "name=\"name\""
            bodyNotContains "error"

        yit "replays invalid input and errors after the redirect, once" $ do
            get FormR
            token <- extractToken
            postParams FormR token [("name", "alice"), ("age", "12")]
            statusIs 303
            get FormR
            statusIs 200
            bodyContains "value=\"alice\""
            bodyContains "value=\"12\""
            bodyContains "Too young"
            get FormR
            statusIs 200
            bodyNotContains "alice"
            bodyNotContains "Too young"

        yit "does not stash on success" $ do
            get FormR
            token <- extractToken
            postParams FormR token [("name", "alice"), ("age", "30")]
            statusIs 303
            get FormR
            statusIs 200
            bodyContains "registered"
            bodyNotContains "value=\"alice\""

        yit "success clears a stale stash" $ do
            get FormR
            token <- extractToken
            get StaleR
            postParams FormR token [("name", "alice"), ("age", "30")]
            statusIs 303
            get FormR
            statusIs 200
            bodyContains "registered"
            bodyNotContains "stale"

        yit "still enforces CSRF on POST" $ do
            get FormR
            postParams FormR "wrong-token" [("name", "alice"), ("age", "30")]
            statusIs 303
            get FormR
            statusIs 200
            bodyNotContains "registered"
            bodyContains "value=\"alice\""

        yit "replays only the submitted one of two identified forms" $ do
            get TwoR
            token <- extractToken
            postParams TwoR token
                [("_formid", "identify-foo"), ("foonum", "foo-banana")]
            statusIs 303
            get TwoR
            statusIs 200
            bodyContains "value=\"foo-banana\""
            bodyContains "foonum: "
            bodyNotContains "barnum: "

        yit "replays the second form on the page too" $ do
            get TwoR
            token <- extractToken
            postParams TwoR token
                [("_formid", "identify-bar"), ("barnum", "bar-banana")]
            statusIs 303
            get TwoR
            statusIs 200
            bodyContains "value=\"bar-banana\""
            bodyContains "barnum: "
            bodyNotContains "foonum: "

        yit "treats an undecodable stash as absent" $ do
            get CorruptR
            statusIs 200
            get FormR
            statusIs 200
            bodyContains "name=\"name\""

        yit "keys the stash by path" $ do
            get FormR
            token <- extractToken
            postParams FormR token [("name", "alice"), ("age", "12")]
            statusIs 303
            get TwoR
            statusIs 200
            bodyNotContains "alice"
            get FormR
            statusIs 200
            bodyContains "value=\"alice\""