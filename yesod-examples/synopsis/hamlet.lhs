\begin{code}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Text.Hamlet
import Yesod.Widget
import Data.Text (Text, cons)
import qualified Data.Text.Lazy.IO as L
import Text.Blaze.Renderer.Text (renderHtml)


data Person = Person
    { name :: String
    , age :: String
    , page :: PersonUrls
    , isMarried :: Bool
    , children :: [String]
    }
data PersonUrls = Homepage | PersonPage Text

renderUrls :: PersonUrls -> [(Text, Text)] -> Text
renderUrls Homepage _ = "/"
renderUrls (PersonPage name) _ = '/' `cons` name

footer :: HtmlUrl url
footer = [$hamlet|\
<div id="footer">Thank you, come again
|]

template :: Person -> HtmlUrl PersonUrls
template person = [$hamlet|
!!!

<html>
    <head>
        <title>Hamlet Demo
    <body>
        <h1>Information on #{name person}
        <p>#{name person} is #{age person} years old.
        <h2>
            $if isMarried person
                \Married
            $else
                \Not married
        <ul>
            $forall child <- children person
                <li>#{child}
        <p>
            <a href="@{page person}">See the page.
        \^{footer}
|]

main :: IO ()
main = do
    let person = Person
            { name = "Michael"
            , age = "twenty five & a half"
            , page = PersonPage "michael"
            , isMarried = True
            , children = ["Adam", "Ben", "Chris"]
            }
    L.putStrLn $ renderHtml $ (template person) renderUrls 
\end{code}

Outputs (new lines added for readability):
<code><pre>
    &lt;!DOCTYPE html&gt;
    &lt;html&gt;&lt;head&gt;&lt;title&gt;Hamlet Demo&lt;/title&gt;&lt;/head&gt;&lt;body&gt;
    &lt;h1&gt;Information on Michael&lt;/h1&gt;
    &lt;p&gt;Michael is twenty five &amp; a half years old.&lt;/p&gt;
    &lt;h2&gt;Married&lt;/h2&gt;
    &lt;ul&gt;&lt;li&gt;Adam&lt;/li&gt;&lt;li&gt;Ben&lt;/li&gt;&lt;li&gt;Chris&lt;/li&gt;&lt;/ul&gt;
    &lt;p&gt;&lt;a href="/michael"&gt;See the page.&lt;/a&gt;&lt;/p&gt;
    &lt;div id="footer"&gt;Thank you, come again&lt;/div&gt;
    &lt;/body&gt;&lt;/html&gt;
</pre></code>
