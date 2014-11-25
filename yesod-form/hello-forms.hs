{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
import Yesod.Core
import Yesod.Form
import Yesod.Form.Nic
import Yesod.Form.MassInput
import Control.Applicative
import Data.Text (Text, pack)
import Network.Wai.Handler.Warp (run)
import Data.Time (utctDay, getCurrentTime)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Text.Blaze.Html.Renderer.String (renderHtml)

data Fruit = Apple | Banana | Pear
    deriving (Show, Enum, Bounded, Eq)

fruits :: [(Text, Fruit)]
fruits = map (\x -> (pack $ show x, x)) [minBound..maxBound]

mkYesod "HelloForms" [parseRoutes|
/ RootR GET
/mass MassR GET
/valid ValidR GET
/file FileR GET POST
|]

myForm = fixType $ runFormGet $ renderDivs $ pure (,,,,,,,,,,,,)
    <*> pure "pure works!"
    <*> areq boolField "Bool field" Nothing
    <*> aopt boolField "Opt bool field" Nothing
    <*> areq textField "Text field" Nothing
    <*> areq (selectFieldList fruits) "Select field" Nothing
    <*> aopt (selectFieldList fruits) "Opt select field" Nothing
    <*> areq (multiSelectFieldList fruits) "Multi select field" Nothing
    <*> aopt (multiSelectFieldList fruits) "Opt multi select field" Nothing
    <*> aopt intField "Opt int field" Nothing
    <*> aopt (radioFieldList fruits) "Opt radio" Nothing
    <*> aopt multiEmailField "Opt multi email" Nothing
    <*> areq nicHtmlField "NIC HTML" Nothing
    <*> aopt timeField "Opt Time" Nothing

instance Show Html where
    show = renderHtml

data HelloForms = HelloForms

instance RenderMessage HelloForms FormMessage where
    renderMessage _ _ = defaultFormMessage

instance Yesod HelloForms
instance YesodNic HelloForms

fixType :: Handler a -> Handler a
fixType = id

getRootR = do
    ((res, form), enctype) <- myForm
    defaultLayout [whamlet|
<p>Result: #{show res}
<form enctype=#{enctype}>
    ^{form}
    <div>
        <input type=submit>
<p>
    <a href=@{MassR}>See the mass form
<p>
    <a href=@{ValidR}>Validation form
<p>
    <a href=@{FileR}>File form
|]

myMassForm = fixType $ runFormGet $ renderTable $ inputList "People" massTable
    (\x -> (,)
        <$> areq textField "Name" (fmap fst x)
        <*> areq intField "Age" (fmap snd x)) (Just [("Michael", 26)])

getMassR = do
    ((res, form), enctype) <- myMassForm
    defaultLayout [whamlet|
<p>Result: #{show res}
<form enctype=#{enctype}>
    <table>
        ^{form}
    <div>
        <input type=submit>
<p>
    <a href=@{RootR}>See the regular form
|]

myValidForm = fixType $ runFormGet $ renderTable $ pure (,,)
    <*> areq (check (\x ->
            if T.length x < 3
                then Left ("Need at least 3 letters" :: Text)
                else Right x
              ) textField)
            "Name" Nothing
    <*> areq (checkBool (>= 18) ("Must be 18 or older" :: Text) intField)
            "Age" Nothing
    <*> areq (checkM inPast dayField) "Anniversary" Nothing
  where
    inPast x = do
        now <- liftIO $ getCurrentTime
        return $ if utctDay now < x
                    then Left ("Need a date in the past" :: Text)
                    else Right x

getValidR = do
    ((res, form), enctype) <- myValidForm
    defaultLayout [whamlet|
<p>Result: #{show res}
<form enctype=#{enctype}>
    <table>
        ^{form}
    <div>
        <input type=submit>
<p>
    <a href=@{RootR}>See the regular form
|]

main = toWaiApp HelloForms >>= run 3000

fileForm = renderTable $ pure (,)
    <*> (FileInfo' <$> areq fileField "Required file" Nothing)
    <*> (fmap FileInfo' <$> aopt fileField "Optional file" Nothing)

newtype FileInfo' = FileInfo' FileInfo

instance Show FileInfo' where
    show (FileInfo' f) = show (fileName f, fileContentType f)

getFileR = do
    ((res, form), enctype) <- runFormPost fileForm
    defaultLayout [whamlet|
<p>Result: #{show res}
<form method=post enctype=#{enctype}>
    <table>
        ^{form}
    <tr>
        <td>
            <input type=submit>
<p>
    <a href=@{RootR}>See the regular form
|]

postFileR = getFileR
