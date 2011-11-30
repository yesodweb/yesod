<p>Well, just about every web framework I've seen starts with a blog tutorial- so here's mine! Actually, you'll see that this is actually a much less featureful blog than most, but gives a good introduction to Yesod basics. I recommend you start by <a href="/book/basics">reading the basics chapter</a>.</p>

<p>This file is literate Haskell, so we'll start off with our language pragmas and import statements. Basically every Yesod application will start off like this:</p>

> {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
> import Yesod

Next, we'll define the blog entry information. Usually, we would want to store the data in a database and allow users to modify them, but we'll simplify for the moment.

> data Entry = Entry
>       { entryTitle :: String
>       , entrySlug :: String -- ^ used in the URL
>       , entryContent :: String
>       }

Since normally you'll need to perform an IO action to load up your entries from a database, we'll define the loadEntries function to be in the IO monad.

> loadEntries :: IO [Entry]
> loadEntries = return
>   [ Entry "Entry 1" "entry-1" "My first entry"
>   , Entry "Entry 2" "entry-2" "My second entry"
>   , Entry "Entry 3" "entry-3" "My third entry"
>   ]

Each Yesod application needs to define the site argument. You can use this for storing anything that should be loaded before running your application. For example, you might store a database connection there. In our case, we'll store our list of entries.

> data Blog = Blog { blogEntries :: [Entry] }

Now we use the first "magical" Yesod set of functions: mkYesod and parseRoutes. If you want to see *exactly* what they do, look at their Haddock docs. For now, we'll try to keep this tutorial simple:

> mkYesod "Blog" [parseRoutes|
> /               HomeR   GET
> /entry/#String  EntryR  GET
> |]

Usually, the next thing you want to do after a call to mkYesod is to create an instance of Yesod. Every Yesod app needs this; it is a centralized place to define some settings. All settings but approot have sensible defaults. In general, you should put in a valid, fully-qualified URL for your approot, but you can sometimes get away with just doing this:

> instance Yesod Blog where approot _ = ""

This only works if you application is being served from the root of your webserver, and if you never use features like sitemaps and atom feeds that need absolute URLs.

We defined two resource patterns for our blog: the homepage, and the page for each entry. For each of these, we are allowing only the GET request method. For the homepage, we want to simply redirect to the most recent entry, so we'll use:

> getHomeR :: Handler ()
> getHomeR = do
>   Blog entries <- getYesod
>   let newest = last entries
>   redirect RedirectTemporary $ EntryR $ entrySlug newest

We go ahead and send a 302 redirect request to the entry resource. Notice how we at no point need to construct a String to redirect to; this is the beauty of type-safe URLs.

Next we'll define a template for entry pages. Normally, I tend to just define them within the handler function, but it's easier to follow if they're separate. Also for clarity, I'll define a datatype for the template arguments. It would also be possible to simply use the Entry datatype with some filter functions, but I'll save that for a later tutorial.

> data TemplateArgs = TemplateArgs
>   { templateTitle :: Html
>   , templateContent :: Html
>   , templateNavbar :: [Nav]
>   }

The Nav datatype will contain navigation information (ie, the URL and title) of each entry.

> data Nav = Nav
>   { navUrl :: Route Blog
>   , navTitle :: Html
>   }

And now the template itself:

> entryTemplate :: TemplateArgs -> HtmlUrl (Route Blog)
> entryTemplate args = [hamlet|
>   !!!
>   <html>
>       <head>
>           <title>#{templateTitle args}
>       <body>
>           <h1>Yesod Sample Blog
>           <h2>#{templateTitle args}
>           <ul id="nav">
>               $forall nav <- templateNavbar args
>                   <li>
>                       <a href="@{navUrl nav}">#{navTitle nav}
>           <div id="content">
>               \#{templateContent args}
>   |]

Hopefully, that is fairly easy to follow; if not, please review the Hamlet documentation. Just remember that dollar signs mean Html variables, and at signs mean URLs.

Finally, the entry route handler:

> getEntryR :: String -> Handler RepHtml
> getEntryR slug = do
>   Blog entries <- getYesod
>   case filter (\e -> entrySlug e == slug) entries of
>       [] -> notFound
>       (entry:_) -> do
>           let nav = reverse $ map toNav entries
>           let tempArgs = TemplateArgs
>                   { templateTitle = toHtml $ entryTitle entry
>                   , templateContent = toHtml $ entryContent entry
>                   , templateNavbar = nav
>                   }
>           hamletToRepHtml $ entryTemplate tempArgs
>  where
>    toNav :: Entry -> Nav
>    toNav e = Nav
>       { navUrl = EntryR $ entrySlug e
>       , navTitle = toHtml $ entryTitle e
>       }

All that's left now is the main function. Yesod is built on top of WAI, so you can use any WAI handler you wish. For the tutorials, we'll use the basicHandler that comes built-in with Yesod: it serves content via CGI if the appropriate environment variables are available, otherwise with simpleserver.

> main :: IO ()
> main = do
>   entries <- loadEntries
>   warpDebug 3000 $ Blog entries

And this is just to avoid some warnings...

> _ignored :: Widget
> _ignored = undefined blogEntries
