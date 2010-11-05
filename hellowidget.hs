{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
import Yesod
import Yesod.Widget
import Yesod.Helpers.Static
import Yesod.Form.Jquery
import Yesod.Form.Core
import Data.Monoid
import Yesod.Form.Nic
import Control.Applicative
import qualified Data.ByteString.Lazy as L
import System.Directory
import Control.Monad.Trans.Class
import Data.Default

data HW = HW { hwStatic :: Static }
mkYesod "HW" [$parseRoutes|
/ RootR GET
/form FormR
/static StaticR Static hwStatic
/autocomplete AutoCompleteR GET
/customform CustomFormR GET
|]
instance Yesod HW where
    approot _ = ""
    addStaticContent ext _ content = do
        let fn = (base64md5 content) ++ '.' : ext
        liftIO $ createDirectoryIfMissing True "static/tmp"
        liftIO $ L.writeFile ("static/tmp/" ++ fn) content
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

type Handler = GHandler HW HW

instance YesodNic HW
instance YesodJquery HW where
    urlJqueryUiCss _ = Right $ googleHostedJqueryUiCss "ui-darkness"
wrapper h = [$hamlet|
#wrapper ^h^
%footer Brought to you by Yesod Widgets&trade;
|]
getRootR = defaultLayout $ wrapper $ do
    i <- newIdent
    setTitle $ string "Hello Widgets"
    addCassius [$cassius|
#$i$
    color: red
|]
    addStylesheet $ StaticR $ StaticRoute ["style.css"] []
    addStylesheetRemote "http://localhost:3000/static/style2.css"
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
    addScript $ StaticR $ StaticRoute ["script.js"] []
    addHamlet [$hamlet|
%h1#$i$ Welcome to my first widget!!!
%p
    %a!href=@RootR@ Recursive link.
%p
    %a!href=@FormR@ Check out the form.
%p
    %a!href=@CustomFormR@ Custom form arrangement.
%p.noscript Your script did not load. :(
|]
    addHtmlHead [$hamlet|%meta!keywords=haskell|]

handleFormR = do
    (res, form, enctype, hidden) <- runFormPost $ fieldsToTable $ (,,,,,,,,,,)
        <$> stringField (FormFieldSettings "My Field" "Some tooltip info" Nothing Nothing) Nothing
        <*> stringField ("Another field") (Just "some default text")
        <*> intField (FormFieldSettings "A number field" "some nums" Nothing Nothing) (Just 5)
        <*> jqueryDayField def
                { jdsChangeMonth = True
                , jdsChangeYear = True
                , jdsYearRange = "1900:c+10"
                , jdsNumberOfMonths = Right (2, 3)
                } ("A day field") Nothing
        <*> timeField ("A time field") Nothing
        <*> boolField FormFieldSettings
                { ffsLabel = "A checkbox"
                , ffsTooltip = ""
                , ffsId = Nothing
                , ffsName = Nothing
                } (Just False)
        <*> jqueryAutocompleteField AutoCompleteR
            (FormFieldSettings "Autocomplete" "Try it!" Nothing Nothing) Nothing
        <*> nicHtmlField ("HTML")
                (Just $ string "You can put rich text here")
        <*> maybeEmailField ("An e-mail addres") Nothing
        <*> maybeTextareaField "A text area" Nothing
        <*> maybeFileField "Any file"
    let (mhtml, mfile) = case res of
                    FormSuccess (_, _, _, _, _, _, _, x, _, _, y) -> (Just x, y)
                    _ -> (Nothing, Nothing)
    let txt = case res of
                    FormSuccess (_, _, _, _, _, _, _, _, _, Just x, _) -> Just x
                    _ -> Nothing
    defaultLayout $ do
        addCassius [$cassius|
.tooltip
    color: #666
    font-style: italic
|]
        addCassius [$cassius|
textarea.html
    width: 300px
    height: 150px
|]
        addWidget [$hamlet|
$maybe formFailures.res failures
    %ul.errors
        $forall failures f
            %li $f$
%form!method=post!enctype=$enctype$
    $hidden$
    %table
        ^form^
        %tr
            %td!colspan=2
                %input!type=submit
    $maybe mhtml html
        $html$
    $maybe txt t
        $t$
    $maybe mfile f
        $show.f$
|]
        setTitle $ string "Form"

main = basicHandler 3000 $ HW $ fileLookupDir "static" typeByExt

getAutoCompleteR :: Handler RepJson
getAutoCompleteR = do
    term <- runFormGet' $ stringInput "term"
    jsonToRepJson $ jsonList
        [ jsonScalar $ term ++ "foo"
        , jsonScalar $ term ++ "bar"
        , jsonScalar $ term ++ "baz"
        ]

data Person = Person String Int
getCustomFormR = do
    let customForm = GForm $ do
            (a1, [b1], c1) <- deform $ stringInput "name"
            (a2, [b2], c2) <- deform $ intInput "age"
            let b = do
                    b1' <- extractBody b1
                    b2' <- extractBody b2
                    addHamlet [$hamlet|
%p This is a custom layout.
%h1 Name Follows!
%p ^b1'^
%p Age: ^b2'^
|]
            return (Person <$> a1 <*> a2, b , c1 `mappend` c2)
    (_, wform, enctype) <- runFormGet customForm
    defaultLayout $ do
        form <- extractBody wform
        addHamlet [$hamlet|
%form
    ^form^
    %div
        %input!type=submit
|]
