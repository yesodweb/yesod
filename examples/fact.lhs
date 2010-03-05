> {-# LANGUAGE QuasiQuotes #-}

I in general recommend type signatures for everything. However, I wanted
to show in this example how it is possible to get away without the
signatures.

> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}

There are only two imports: Yesod includes all of the code we need for creating
a web application, while Network.Wai.Handler.SimpleServer allows us to test our
application easily. A Yesod app can in general run on any WAI handler, so this
application is easily convertible to CGI, FastCGI, or even run on the Happstack
server.

> import Yesod
> import Network.Wai.Handler.SimpleServer

The easiest way to start writing a Yesod app is to follow the Yesod typeclass.
You define some data type which will contain all the specific settings and data
you want in your application. This might include database connections,
templates, etc. It's entirely up to you.

For our simple demonstration, we need no extra data, so we simply define Fact
as:

> data Fact = Fact

Now we need to declare an instance of Yesod for Fact. The most important
function to declare is handlers, which defines which functions deal with which
resources (aka URLs).

You can declare the function however you want, but Yesod.Resource declares a
convenient "resources" quasi-quoter which takes YAML content and generates the
function for you. There is a lot of cool stuff to do with representations going
on here, but this is not the appropriate place to discuss it.


> instance Yesod Fact where

The structure is very simply: top level key is a "resource pattern". A resource pattern is simply a bunch of slash-separated strings, called "resource pattern pieces". There are three special ways to start a piece:

* $: will take any string

* \#: will take any integer

* \*: will "slurp" up all the remaining pieces. Useful for something like
     /static/*filepath

Otherwise, the piece is treated as a literal string which must be matched.


Now we have a mapping of verbs to handler functions. We could instead simply
specify a single function which handles all verbs. (Note: a verb is just a
request method.)

\begin{code}
   resources = [$mkResources|
/:
    GET: index
/#num:
    GET: fact
/fact:
    GET: factRedirect
|]
\end{code}

This does what it looks like: serves a static HTML file.

> index = sendFile TypeHtml "examples/fact.html" >> return ()

HtmlObject is a funny beast. Basically, it allows multiple representations of
data, all with HTML entities escaped properly. These representations include:

* Simple HTML document (only recommended for testing).
* JSON (great for Ajax)
* Input to a HStringTemplate (great for no-Javascript fallback).

For simplicity here, we don't include a template, though it would be trivial to
do so (see the hellotemplate example).

> fact i = applyLayoutJson "Factorial result" $ cs
>             [ ("input", show i)
>             , ("result", show $ product [1..fromIntegral i :: Integer])
>             ]

I've decided to have a redirect instead of serving the some data in two
locations. It fits in more properly with the RESTful principal of one name for
one piece of data.

> factRedirect :: Handler y ()
> factRedirect = do
>     rr <- getRequest
>     let i = case getParams rr "num" of -- FIXME
>               [] -> "1"
>               (x:_) -> x
>     _ <- redirect RedirectPermanent $ "../" ++ i ++ "/"

The following line would be unnecesary if we had a type signature on
factRedirect.

>     return ()

You could replace this main to use any WAI handler you want. For production,
you could use CGI, FastCGI or a more powerful server. Just check out Hackage
for options (any package starting hack-handler- should suffice).

> main :: IO ()
> main = putStrLn "Running..." >> toWaiApp Fact >>= run 3000
