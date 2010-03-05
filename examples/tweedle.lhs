#!/usr/bin/env runhaskell
> {-# LANGUAGE QuasiQuotes #-}

While coming up on the first release of Yesod, I realized I needed a nice, comprehensive tutorial. I didn't want to do the typical blog example, since it's so trite. I considered doing a Reddit or Twitter clone (the former became a bit of a meme a few weeks ago), but then I needed to set up a bug tracker for some commercial projects I was working on, and I decided that it would be a great example program.

Before getting started, a quick word of warning: Yesod at this point really provides nothing in terms of data storage (aka, the model). There is wonderful integration with the data-object package, and the data-object-yaml package provides good serialization, but this is all very inefficient in practice. For simplicity, I've gone ahead and used this as the storage model; this should not be done for production code.

There's a lot of boilerplate code at the beginning that just has to do with object storage; if you'd like to skip it, just start reading from the main function.

Anyway, here's the import list.

> import Yesod
> import Yesod.Helpers.Auth
> import Data.Object.Yaml
> import Data.Object.String
> import Control.Concurrent
> import qualified Safe.Failure as SF
> import Data.Time
> import Data.Attempt (Attempt, fromAttempt)
> import Control.Arrow (second)
> import qualified Network.Wai.Handler.SimpleServer
> import Data.Monoid
> import Data.Text (pack)
> import Control.Applicative ((<$>), (<*>))

One of the goals of Yesod is to make it work with the compiler to help you program. Instead of configuration files, it uses typeclasses to both change default behavior and enable extra features. An example of the former is error pages, while an example of the latter is authentication.

To start with, we need a datatype to represent our program. We'll call this bug tracker "Tweedle", after Dr. Seuss's "Tweedle Beetle Battle" in "Fox in Socks" (my son absolutely loves this book). We'll be putting the complete state of the bug database in an MVar within this variable; in a production setting, you might instead put a database handle.

> data Tweedle = Tweedle Settings (MVar Category) TemplateGroup

(For now, just ignore the TemplateGroup, its purpose becomes apparent later.)

This issue database is fully hierarchical: each category can contain subcategories and issues. This might be too much nesting for many uses, but it's what my project demanded.

Also, if I cared about efficiency here, a trie or map would probably be a better data structure. As stated above, it doesn't matter.

> data Category = Category
>   { subCats :: [Category]
>   , subIssues :: [Issue]
>   , categoryId :: Integer
>   , catName :: String
>   }

> data Issue = Issue
>   { issueName :: String
>   , issueMessages :: [Message]
>   , issueId :: Integer
>   }

Further simplifications: authors will just be represented by their OpenID URL.

> data Message = Message
>   { messageAuthor :: OpenId
>   , messageStatus :: Maybe String
>   , messagePriority :: Maybe String
>   , messageText :: String
>   , messageCreation :: UTCTime
>   }

> type OpenId = String

We need to be able to serialize this data to and from YAML files. You can consider all of the following code boilerplate.

> messageToSO :: Message -> StringObject
> messageToSO m = Mapping $ map (second Scalar)
>     [ ("author", messageAuthor m)
>     , ("status", show $ messageStatus m)
>     , ("priority", show $ messagePriority m)
>     , ("text", messageText m)
>     , ("creation", show $ messageCreation m)
>     ]
> messageFromSO :: StringObject -> Attempt Message
> messageFromSO so = do
>     m <- fromMapping so
>     a <- lookupScalar "author" m
>     s <- lookupScalar "status" m >>= SF.read
>     p <- lookupScalar "priority" m >>= SF.read
>     t <- lookupScalar "text" m
>     c <- lookupScalar "creation" m >>= SF.read
>     return $ Message a s p t c
> issueToSO :: Issue -> StringObject
> issueToSO i = Mapping
>   [ ("name", Scalar $ issueName i)
>   , ("messages", Sequence $ map messageToSO $ issueMessages i)
>   , ("id", Scalar $ show $ issueId i)
>   ]
> issueFromSO :: StringObject -> Attempt Issue
> issueFromSO so = do
>   m <- fromMapping so
>   n <- lookupScalar "name" m
>   i <- lookupScalar "id" m >>= SF.read
>   ms <- lookupSequence "messages" m >>= mapM messageFromSO
>   return $ Issue n ms i
> categoryToSO :: Category -> StringObject
> categoryToSO c = Mapping
>   [ ("cats", Sequence $ map categoryToSO $ subCats c)
>   , ("issues", Sequence $ map issueToSO $ subIssues c)
>   , ("id", Scalar $ show $ categoryId c)
>   , ("name", Scalar $ catName c)
>   ]
> categoryFromSO :: StringObject -> Attempt Category
> categoryFromSO so = do
>   m <- fromMapping so
>   cats <- lookupSequence "cats" m >>= mapM categoryFromSO
>   issues <- lookupSequence "issues" m >>= mapM issueFromSO
>   i <- lookupScalar "id" m >>= SF.read
>   n <- lookupScalar "name" m
>   return $ Category cats issues i n

Well, that was a mouthful. You can safely ignore all of that: it has nothing to do with actual web programming.

Next is the Settings datatype. Normally I create a settings file so I can easily make changes between development and production systems without recompiling, but once again we are aiming for simplicity here.

> data Settings = Settings

Many web frameworks make the simplifying assumptions that "/" will be the path to the root of your application. In real life, this doesn't always happen. In Yesod, you must specify explicitly your application root and then create an instance of YesodApproot (see below). Again, the compiler will let you know this: once you use a feature that depends on knowing the approot, you'll get a compiler error if you haven't created the instance.

>   { sApproot :: String
>   , issueFile :: FilePath

Yesod comes built in with support for HStringTemplate. You'll see later how this ties in with data-object (and in particular HtmlObject) to help avoid XSS attacks.

>   , templatesDir :: FilePath
>   }

And now we'll hardcode the settings instead of loading from a file. I'll do it in the IO monad anyway, since that would be the normal function signature.

> loadSettings :: IO Settings
> loadSettings = return $ Settings "http://localhost:3000/" "issues.yaml" "examples/tweedle-templates"

And now we need a function to load up our Tweedle data type.

> loadTweedle :: IO Tweedle
> loadTweedle = do
>   settings <- loadSettings

Note that this will die unless an issues file is present. We could instead check for the file and create it if missing, but instead, just put the following into issues.yaml:

{cats: [], issues: [], id: 0, name: "Top Category"}

>   issuesSO <- decodeFile $ issueFile settings
>   issues <- fromAttempt $ categoryFromSO issuesSO
>   missues <- newMVar issues
>   tg <- loadTemplateGroup $ templatesDir settings
>   return $ Tweedle settings missues tg

And now we're going to write our main function. Yesod is built on top of the Web Application Interface (wai package), so a Yesod application runs on a variety of backends. For our purposes, we're going to use the SimpleServer.

> main :: IO ()
> main = do
>   putStrLn "Running at http://localhost:3000/"
>   tweedle <- loadTweedle
>   app <- toWaiApp tweedle
>   Network.Wai.Handler.SimpleServer.run 3000 app

Well, that was a *lot* of boilerplate code that had nothing to do with web programming. Now the real stuff begins. I would recommend trying to run the code up to now an see what happens. The compiler will complain that there is no instance of Yesod for Tweedle. This is what I meant by letting the compiler help us out. So now we've got to create the Yesod instance.

The Yesod typeclass includes many functions, most of which have default implementations. I'm not going to go through all of them here, please see the documentation.

> instance Yesod Tweedle where

The most important function is resources: this is where all of the URL mapping will occur. Yesod adheres to Restful principles very strongly. A "resource" is essentially a URL. Each resource should be unique; for example, do not create /user/5/ as well as /user/by-number/5/. In addition to resources, we also determine which function should handle your request based on the request method. In other words, a POST and a GET are completely different.

One of the middlewares that Yesod installs is called MethodOverride; please see the documentation there for more details, but essentially it allows us to work past a limitation in the form tag of HTML to use PUT and DELETE methods as well.

Instead of using regular expressions to handle the URL mapping, Yesod uses resource patterns. A resource is a set of tokens separated by slashes. Each of those tokens can be one of:

* A static string.
* An integer variable (begins with #), which will match any integer.
* A string varaible (begins with $), which will match any single value.
* A "slurp" variable, which will match all of the remaining tokens. It must be the last token.

Yesod uses quasi quotation to make specifying the resource pattern simple and safe: your entire set of patterns is checked at compile time to see if you have overlapping rules.

>   resources = [$mkResources|

Now we need to figure out all of the resources available in our application. We'll need a homepage:

>   /:
>       GET: homepageH

We will also need to allow authentication. We use the slurp pattern here and accept all request methods. The authHandler method (in the Yesod.Helpers.Auth module) will handle everything itself.

>   /auth/*: authHandler

We're going to refer to categories and issues by their unique numerical id. We're also going to make this system append only: there is no way to change the history.

>   /category/#id: # notice that "id" is ignored by Yesod
>       GET: categoryDetailsH
>       PUT: createCategoryH
>   /category/#id/issues:
>       PUT: createIssueH
>   /issue/#id:
>       GET: issueDetailsH
>       PUT: addMessageH
>   |]

So if you make a PUT request to "/category/5", you will be creating a subcategory of category 5. "GET /issue/27/" will display details on issue 27. This is all we need.

If you try to compile the code until this point, the compiler will tell you that you have to define all of the above-mentioned functions. We'll do that in a second; for now, if you'd like to see the rest of the error messages, uncomment this next block of code.

> {-
> homepageH = return ()
> categoryDetailsH _ = return ()
> createCategoryH _ = return ()
> createIssueH _ = return ()
> issueDetailsH _ = return ()
> addMessageH _ = return ()
> -}

Now the compiler is telling us that there's no instance of YesodAuth for Tweedle. YesodAuth- as you might imagine- keeps settings on authentication. We're going to go ahead a create an instance now. The default settings work if you set up authHandler for "/auth/*" (which we did) and are using openid (which we are). So all we need to do is:

> instance YesodAuth Tweedle

Running that tells us that we're missing a YesodApproot instance as well. That's easy enough to fix:

> instance YesodApproot Tweedle where
>   approot (Tweedle settings _ _) = sApproot settings

Congratulations, you have a working web application! Gratned, it doesn't actually do much yet, but you *can* use it to log in via openid. Just go to http://localhost:3000/auth/openid/.

Now it's time to implement the real code here. We'll start with the homepage. For this program, I just want the homepage to redirect to the main category (which will be category 0). So let's create that redirect:

> homepageH :: Handler Tweedle ()
> homepageH = do
>   ar <- getApproot
>   redirect RedirectPermanent $ ar ++ "category/0/"

Simple enough. Notice that we used the getApproot function; if we wanted, we could have just assumed the approot was "/", but this is more robust.

Now the category details function. We're just going to have two lists: subcategories and direct subissues. Each one will have a name and numerical ID.

But here's a very nice feature of Yesod: We're going to have multiple representations of this data. The main one people will use is the HTML representation. However, we're also going to provide a JSON representation. This will make it very simple to write clients or to AJAXify this application in the future.

> categoryDetailsH :: Integer -> Handler Tweedle RepHtmlJson

That function signature tells us a lot: the parameter is the category ID, and we'll be returning something that has both an HTML and JSON representation.

> categoryDetailsH catId = do

getYesod returns our Tweedle data type. Remember, we wrapped it in an MVar; since this is a read-only operation, will unwrap the MVar immediately.

>   Tweedle _ mvarTopCat _ <- getYesod
>   topcat <- liftIO $ readMVar mvarTopCat

Next we need to find the requested category. You'll see the (boilerplate) function below. If the category doesn't exist, we want to return a 404 response page. So:

>   (parents, cat) <- maybe notFound return $ findCat catId [] topcat

Now we want to convert the category into an HtmlObject. By doing so, we will get automatic HTML entity encoding; in other words, no XSS attacks.

>   let catHelper (Category _ _ cid name) = Mapping
>           [ ("name", Scalar $ Text $ pack name)
>           , ("id", Scalar $ Text $ pack $ show cid)
>           ]
>   let issueHelper (Issue name _ iid) = Mapping
>           [ ("name", Scalar $ Text $ pack name)
>           , ("id", Scalar $ Text $ pack $ show iid)
>           ]
>   let ho = Mapping
>           [ ("cats", Sequence $ map catHelper $ subCats cat)
>           , ("issues", Sequence $ map issueHelper $ subIssues cat)
>           ]

And now we'll use a String Template to display the whole thing.

>   templateHtmlJson "category-details" ho $ \_ -> return
>       . setHtmlAttrib "cat" ho
>       . setHtmlAttrib "name" (catName cat)
>       . setHtmlAttrib "parents" (Sequence $ map catHelper parents)

> findCat :: Integer -> [Category] -> Category -> Maybe ([Category], Category)
> findCat i parents c@(Category cats _ i' _)
>     | i == i' = Just (parents, c)
>     | otherwise = getFirst $ mconcat $ map (First . findCat i (parents ++ [c])) cats

Now we get a new missing instance: YesodTemplate. As you can imagine, this is because of calling the templateHtmlJson function. This is easily enough solved (and explains why we needed TemplateGroup as part of Tweedle).

> instance YesodTemplate Tweedle where
>   getTemplateGroup (Tweedle _ _ tg) = tg

Now we actually get some output! I'm not going to cover the syntax of string templates here, but you should read the files in the examples/tweedle-templates directory.

Next, we need to implement createCategoryH. There are two parts to this process: parsing the form submission, and then modifying the database. Pay attention to the former, but you can ignore the latter if you wish. Also, this code does not do much for error checking, as that would needlessly complicate matters.

> createCategoryH :: Integer -> Handler Tweedle ()
> createCategoryH parentid = do

Yesod uses a formlets-style interface for parsing submissions. This following line says we want a parameter named catname, which precisely one value (required) and that value must have a non-zero length (notEmpty).

>   catname <- runFormPost $ notEmpty $ required $ input "catname"
>   newid <- modifyDB $ createCategory parentid catname
>   ar <- getApproot
>   redirect RedirectPermanent $ ar ++ "category/" ++ show newid ++ "/"

And here's the database modification code we need. Once again, this is not web-specific.

> modifyDB :: (Category -> (Category, x)) -> Handler Tweedle x
> modifyDB f = do
>   Tweedle settings mcat _ <- getYesod
>   liftIO $ modifyMVar mcat $ \cat -> do
>       let (cat', x) = f cat
>       encodeFile (issueFile settings) $ categoryToSO cat'
>       return (cat', x)

> createCategory :: Integer -> String -> Category -> (Category, Integer)
> createCategory parentid catname topcat =
>   let newid = highCatId topcat + 1
>       topcat' = addChild parentid (Category [] [] newid catname) topcat
>    in (topcat', newid)
>     where
>       highCatId (Category cats _ i _) = maximum $ i : map highCatId cats
>       addChild i' newcat (Category cats issues i name)
>           | i' /= i = Category (map (addChild i' newcat) cats) issues i name
>           | otherwise = Category (cats ++ [newcat]) issues i name

Next is creating an issue. This is almost identical to creating a category.

> createIssueH :: Integer -> Handler Tweedle ()
> createIssueH catid = do
>   issuename <- runFormPost $ notEmpty $ required $ input "issuename"
>   newid <- modifyDB $ createIssue catid issuename
>   ar <- getApproot
>   redirect RedirectPermanent $ ar ++ "issue/" ++ show newid ++ "/"

> createIssue :: Integer -> String -> Category -> (Category, Integer)
> createIssue catid issuename topcat =
>   let newid = highIssueId topcat + 1
>       topcat' = addIssue catid (Issue issuename [] newid) topcat
>    in (topcat', newid)
>     where
>       highIssueId (Category cats issues _ _) =
>           maximum $ 0 : (map issueId issues) ++ map highIssueId cats
>       addIssue i' newissue (Category cats issues i name)
>           | i' /= i = Category (map (addIssue i' newissue) cats) issues i name
>           | otherwise = Category cats (issues ++ [newissue]) i name

Two functions to go. Now we want to show details of issues. This isn't too different from categoryDetailsH above, except for one feature: we need to know if a user is logged in. If they are logged in, we'll show an "add message" box; otherwise, we'll show a login box. Once again, we're getting the JSON representation easily.

> issueDetailsH :: Integer -> Handler Tweedle RepHtmlJson
> issueDetailsH iid = do
>   Tweedle _ mvarTopCat _ <- getYesod
>   topcat <- liftIO $ readMVar mvarTopCat
>   (cat, issue) <- maybe notFound return $ findIssue iid topcat
>   let messageHelper m = Mapping $ map (second $ Scalar . Text . pack)
>         $ (maybe id (\x -> (:) ("status", x)) $ messageStatus m)
>         $ (maybe id (\x -> (:) ("priority", x)) $ messagePriority m)
>         [ ("author", messageAuthor m)
>         , ("text", messageText m)
>         , ("creation", show $ messageCreation m)
>         ]
>   let ho = Mapping
>           [ ("name", Scalar $ Text $ pack $ issueName issue)
>           , ("messages", Sequence $ map messageHelper $ issueMessages issue)
>           ]

Now we determine is the user is logged in via the maybeIdentifier function. Later on, we'll see how we can force a user to be logged in using authIdentifier.

>   ident <- maybeIdentifier

>   templateHtmlJson "issue-details" ho $ \_ -> return
>       . setHtmlAttrib "issue" ho
>       . maybe id (setHtmlAttrib "ident") ident
>       . setHtmlAttrib "cat" (Mapping
>           [ ("name", Scalar $ Text $ pack $ catName cat)
>           , ("id", Scalar $ Text $ pack $ show $ categoryId cat)
>           ])

And now the supporting model code. This function returns the requested Issue along with the containing category.

> findIssue :: Integer -> Category -> Maybe (Category, Issue)
> findIssue iid c@(Category cats issues _ _) =
>   case filter (\issue -> issueId issue == iid) issues of
>       [] -> getFirst $ mconcat $ map (First . findIssue iid) cats
>       (issue:_) -> Just (c, issue)

Cool, just one function left! This should probably all make sense by now. Notice, however, the use of authIdentifier: if the user is not logged in, they will be redirected to the login page automatically.

> addMessageH :: Integer -> Handler Tweedle ()
> addMessageH issueid = do
>   ident <- authIdentifier
>   (status, priority, text) <- runFormPost $
>           (,,)
>       <$> optional (input "status")
>       <*> optional (input "priority")
>       <*> required (input "text")
>   now <- liftIO getCurrentTime
>   let message = Message ident status priority text now
>   modifyDB $ addMessage issueid message
>   ar <- getApproot
>   redirect RedirectPermanent $ ar ++ "issue/" ++ show issueid ++ "/"

> addMessage :: Integer -> Message -> Category -> (Category, ())
> addMessage issueid message (Category cats issues catid catname) =
>   (Category (map (fst . addMessage issueid message) cats) (map go issues) catid catname, ())
>     where
>       go (Issue name messages iid)
>           | iid == issueid = Issue name (messages ++ [message]) iid
>           | otherwise = Issue name messages iid
