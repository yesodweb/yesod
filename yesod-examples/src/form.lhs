<p>Forms can be a tedious part of web development since they require synchronization of code in many different areas: the HTML form declaration, parsing of the form and reconstructing a datatype from the raw values. The Yesod form library simplifies things greatly. We'll start off with a basic application.</p>


> {-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings, MultiParamTypeClasses, TemplateHaskell #-}
> import Yesod
> import Control.Applicative
> import Data.Text (Text)

> data FormExample = FormExample
> mkYesod "FormExample" [parseRoutes|
> / RootR GET
> |]
> instance Yesod FormExample where approot _ = ""
> instance RenderMessage FormExample FormMessage where
>    renderMessage _ _ = defaultFormMessage

Next, we'll declare a Person datatype with a name and age. After that, we'll create a formlet. A formlet is a declarative approach to forms. It takes a Maybe value and constructs either a blank form, a form based on the original value, or a form based on the values submitted by the user. It also attempts to construct a datatype, failing on validation errors.

> data Person = Person { name :: Text, age :: Int }
>     deriving Show
> personFormlet p = renderTable $ Person
>     <$> areq textField "Name" (fmap name p)
>     <*> areq intField "Age" (fmap age p)

We use an applicative approach and stay mostly declarative. The "fmap name p" bit is just a way to get the name from within a value of type "Maybe Person".

> getRootR :: Handler RepHtml
> getRootR = do
>     ((res, wform), enctype) <- runFormGet $ personFormlet Nothing

<p>We use runFormGet to bind to GET (query-string) parameters; we could also use runFormPost. The "Nothing" is the initial value of the form. You could also supply a "Just Person" value if you like. There is a three-tuple returned, containing the parsed value, the HTML form as a widget and the encoding type for the form.</p>

<p>We use a widget for the form since it allows embedding CSS and Javascript code in forms directly. This allows unobtrusive adding of rich Javascript controls like date pickers.</p>

>     defaultLayout $ do
>         setTitle "Form Example"
>         form <- extractBody wform

<p>extractBody returns the HTML of a widget and "passes" all of the other declarations (the CSS, Javascript, etc) up to the parent widget. The rest of this is just standard Hamlet code and our main function.</p>

>         addHamlet [$hamlet|
> <p>Last result: #{show res}
> <form enctype="#{enctype}">
>     <table>
>         \^{form}
>         <tr>
>             <td colspan="2">
>                 <input type="submit">
> |]
> 
> main = warpDebug 3000 FormExample
