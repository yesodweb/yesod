{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Yesod.Test.Mock is identical to Yesod.Test with two exceptions:
  1) Some types are extended to include a mocks field.
  2) A 'getMocks' function is provided so that that field can be recovered.
-}

module Yesod.Test.Mock
    ( -- * Declaring and running your test suite
      yesodSpec
    , YesodSpec
    , yesodSpecWithSiteGenerator
    , yesodSpecApp
    , YesodExample
    , YesodExampleData(..)
    , TestApp
    , YSpec
    , testApp
    , YesodSpecTree (..)
    , ydescribe
    , yit

    -- * Making requests
    -- | You can construct requests with the 'RequestBuilder' monad, which lets you
    -- set the URL and add parameters, headers, and files. Helper functions are provided to
    -- lookup fields by label and to add the current CSRF token from your forms.
    -- Once built, the request can be executed with the 'request' method.
    --
    -- Convenience functions like 'get' and 'post' build and execute common requests.
    , get
    , post
    , postBody
    , followRedirect
    , request
    , addRequestHeader
    , setMethod
    , addPostParam
    , addGetParam
    , addFile
    , setRequestBody
    , RequestBuilder
    , setUrl

    -- *** Adding fields by label
    -- | Yesod can auto generate field names, so you are never sure what
    -- the argument name should be for each one of your inputs when constructing
    -- your requests. What you do know is the /label/ of the field.
    -- These functions let you add parameters to your request based
    -- on currently displayed label names.
    , byLabel
    , fileByLabel

    -- *** CSRF Tokens
    -- | In order to prevent CSRF exploits, yesod-form adds a hidden input
    -- to your forms with the name "_token". This token is a randomly generated,
    -- per-session value.
    --
    -- In order to prevent your forms from being rejected in tests, use one of
    -- these functions to add the token to your request.
    , addToken
    , addToken_
    , addTokenFromCookie
    , addTokenFromCookieNamedToHeaderNamed

    -- * Assertions
    , assertEqual
    , assertEqualNoShow
    , assertEq

    , assertHeader
    , assertNoHeader
    , statusIs
    , bodyEquals
    , bodyContains
    , bodyNotContains
    , htmlAllContain
    , htmlAnyContain
    , htmlNoneContain
    , htmlCount

    -- * Grab information
    , getTestYesod
    , getResponse
    , getRequestCookies
    , getMocks

    -- * Debug output
    , printBody
    , printMatches

    -- * Utils for building your own assertions
    -- | Please consider generalizing and contributing the assertions you write.
    , htmlQuery
    , parseHTML
    , withResponse
    ) where

import qualified Test.Hspec.Core.Spec as Hspec
import qualified Data.List as DL
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Test.HUnit as HUnit
import qualified Network.HTTP.Types as H
import qualified Network.Socket.Internal as Sock
import Data.CaseInsensitive (CI)
import Network.Wai
import Network.Wai.Test hiding (assertHeader, assertNoHeader, request)
import qualified Control.Monad.Trans.State as ST
import Control.Monad.IO.Class
import System.IO
import Yesod.Test.TransversingCSS
import Yesod.Core
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as C
import qualified Text.HTML.DOM as HD
import Control.Monad.Trans.Writer
import qualified Data.Map as M
import qualified Web.Cookie as Cookie
import qualified Blaze.ByteString.Builder as Builder
import Data.Time.Clock (getCurrentTime)
import Control.Applicative ((<$>))
import Text.Show.Pretty (ppShow)
import Yesod.Test.Internal ((<>), addFile, addGetParam, addPostParam,
    addRequestHeader, addToken, addToken_, addTokenFromCookie,
    addTokenFromCookieNamedToHeaderNamed, byLabel, contains, Cookies(..),
    failure, fileByLabel, getRequestCookies, htmlQuery', nameFromLabel,
    parseHTML, parseSetCookies, RBDPostData(..), RequestBuilder(..),
    RequestBuilderData(..), RequestPart(..), setMethod, setRequestBody,
    setUrl, withResponse')


-- | The state used in a single test case defined using 'yit'
--
-- Since 1.2.4
data YesodExampleData site mocks = YesodExampleData
    { yedApp :: !Application
    , yedSite :: !site
    , yedCookies :: !Cookies
    , yedResponse :: !(Maybe SResponse)
    , yedMocks :: !mocks
    }

-- | A single test case, to be run with 'yit'.
--
-- Since 1.2.0
type YesodExample site mocks = ST.StateT (YesodExampleData site mocks) IO

-- | Corresponds to hspec\'s 'Spec'.
--
-- Since 1.2.0
type YesodSpec site mocks = Writer [YesodSpecTree site mocks] ()

-- | Internal data structure, corresponding to hspec\'s 'YesodSpecTree'.
--
-- Since 1.2.0
data YesodSpecTree site mocks
    = YesodSpecGroup String [YesodSpecTree site mocks]
    | YesodSpecItem String (YesodExample site mocks ())

-- | Get the foundation value used for the current test.
--
-- Since 1.2.0
getTestYesod :: YesodExample site mocks site
getTestYesod = yedSite <$> ST.get

-- | Get the most recently provided response value, if available.
--
-- Since 1.2.0
getResponse :: YesodExample site mocks (Maybe SResponse)
getResponse = yedResponse <$> ST.get

-- | Get the mocks used for the current test.
getMocks :: YesodExample site mocks mocks
getMocks = yedMocks <$> ST.get

-- | Start describing a Tests suite keeping cookies and a reference to the tested 'Application'
-- and 'ConnectionPool'
ydescribe :: String -> YesodSpec site mocks -> YesodSpec site mocks
ydescribe label yspecs = tell [YesodSpecGroup label $ execWriter yspecs]

yesodSpec :: YesodDispatch site
          => site
          -> mocks
          -> YesodSpec site mocks
          -> Hspec.Spec
yesodSpec site mocks yspecs =
    Hspec.fromSpecList $ map unYesod $ execWriter yspecs
  where
    unYesod (YesodSpecGroup x y) = Hspec.specGroup x $ map unYesod y
    unYesod (YesodSpecItem x y) = Hspec.specItem x $ do
        app <- toWaiAppPlain site
        ST.evalStateT y YesodExampleData
            { yedApp = app
            , yedSite = site
            , yedCookies = M.empty
            , yedResponse = Nothing
            , yedMocks = mocks
            }

-- | Same as yesodSpec, but instead of taking already built site it
-- takes an action which produces site for each test.
yesodSpecWithSiteGenerator :: YesodDispatch site
                           => IO site
                           -> mocks
                           -> YesodSpec site mocks
                           -> Hspec.Spec
yesodSpecWithSiteGenerator getSiteAction mocks yspecs =
    Hspec.fromSpecList $ map (unYesod getSiteAction) $ execWriter yspecs
    where
      unYesod getSiteAction' (YesodSpecGroup x y) = Hspec.specGroup x $ map (unYesod getSiteAction') y
      unYesod getSiteAction' (YesodSpecItem x y) = Hspec.specItem x $ do
        site <- getSiteAction'
        app <- toWaiAppPlain site
        ST.evalStateT y YesodExampleData
            { yedApp = app
            , yedSite = site
            , yedCookies = M.empty
            , yedResponse = Nothing
            , yedMocks = mocks
            }

-- | Same as yesodSpec, but instead of taking a site it
-- takes an action which produces the 'Application' for each test.
-- This lets you use your middleware from makeApplication
yesodSpecApp :: YesodDispatch site
             => site
             -> IO Application
             -> mocks
             -> YesodSpec site mocks
             -> Hspec.Spec
yesodSpecApp site getApp mocks yspecs =
    Hspec.fromSpecList $ map unYesod $ execWriter yspecs
  where
    unYesod (YesodSpecGroup x y) = Hspec.specGroup x $ map unYesod y
    unYesod (YesodSpecItem x y) = Hspec.specItem x $ do
        app <- getApp
        ST.evalStateT y YesodExampleData
            { yedApp = app
            , yedSite = site
            , yedCookies = M.empty
            , yedResponse = Nothing
            , yedMocks = mocks
            }

-- | Describe a single test that keeps cookies, and a reference to the last response.
yit :: String -> YesodExample site mocks () -> YesodSpec site mocks
yit label example = tell [YesodSpecItem label example]

-- | Performs a given action using the last response. Use this to create
-- response-level assertions
withResponse :: (SResponse -> YesodExample site mocks a) -> YesodExample site mocks a
withResponse = withResponse' yedResponse []

-- | Query the last response using CSS selectors, returns a list of matched fragments
htmlQuery :: Query -> YesodExample site mocks [HtmlLBS]
htmlQuery = htmlQuery' yedResponse []

-- | Asserts that the two given values are equal.
--
-- In case they are not equal, error mesasge includes the two values.
--
-- @since 1.5.2
assertEq :: (Eq a, Show a) => String -> a -> a -> YesodExample site mocks ()
assertEq m a b =
  liftIO $ HUnit.assertBool msg (a == b)
  where msg = "Assertion: " ++ m ++ "\n" ++
              "First argument:  " ++ ppShow a ++ "\n" ++
              "Second argument: " ++ ppShow b ++ "\n"

{-# DEPRECATED assertEqual "Use assertEq instead" #-}
assertEqual :: (Eq a) => String -> a -> a -> YesodExample site mocks ()
assertEqual = assertEqualNoShow

-- | Asserts that the two given values are equal.
--
-- @since 1.5.2
assertEqualNoShow :: (Eq a) => String -> a -> a -> YesodExample site mocks ()
assertEqualNoShow msg a b = liftIO $ HUnit.assertBool msg (a == b)

-- | Assert the last response status is as expected.
statusIs :: Int -> YesodExample site mocks ()
statusIs number = withResponse $ \ SResponse { simpleStatus = s } ->
  liftIO $ flip HUnit.assertBool (H.statusCode s == number) $ concat
    [ "Expected status was ", show number
    , " but received status was ", show $ H.statusCode s
    ]

-- | Assert the given header key/value pair was returned.
assertHeader :: CI BS8.ByteString -> BS8.ByteString -> YesodExample site mocks ()
assertHeader header value = withResponse $ \ SResponse { simpleHeaders = h } ->
  case lookup header h of
    Nothing -> failure $ T.pack $ concat
        [ "Expected header "
        , show header
        , " to be "
        , show value
        , ", but it was not present"
        ]
    Just value' -> liftIO $ flip HUnit.assertBool (value == value') $ concat
        [ "Expected header "
        , show header
        , " to be "
        , show value
        , ", but received "
        , show value'
        ]

-- | Assert the given header was not included in the response.
assertNoHeader :: CI BS8.ByteString -> YesodExample site mocks ()
assertNoHeader header = withResponse $ \ SResponse { simpleHeaders = h } ->
  case lookup header h of
    Nothing -> return ()
    Just s  -> failure $ T.pack $ concat
        [ "Unexpected header "
        , show header
        , " containing "
        , show s
        ]

-- | Assert the last response is exactly equal to the given text. This is
-- useful for testing API responses.
bodyEquals :: String -> YesodExample site mocks ()
bodyEquals text = withResponse $ \ res ->
  liftIO $ HUnit.assertBool ("Expected body to equal " ++ text) $
    (simpleBody res) == encodeUtf8 (TL.pack text)

-- | Assert the last response has the given text. The check is performed using the response
-- body in full text form.
bodyContains :: String -> YesodExample site mocks ()
bodyContains text = withResponse $ \ res ->
  liftIO $ HUnit.assertBool ("Expected body to contain " ++ text) $
    (simpleBody res) `contains` text

-- | Assert the last response doesn't have the given text. The check is performed using the response
-- body in full text form.
-- @since 1.5.3
bodyNotContains :: String -> YesodExample site mocks ()
bodyNotContains text = withResponse $ \ res ->
  liftIO $ HUnit.assertBool ("Expected body not to contain " ++ text) $
    not $ contains (simpleBody res) text

-- | Queries the HTML using a CSS selector, and all matched elements must contain
-- the given string.
htmlAllContain :: Query -> String -> YesodExample site mocks ()
htmlAllContain query search = do
  matches <- htmlQuery query
  case matches of
    [] -> failure $ "Nothing matched css query: " <> query
    _ -> liftIO $ HUnit.assertBool ("Not all "++T.unpack query++" contain "++search) $
          DL.all (DL.isInfixOf search) (map (TL.unpack . decodeUtf8) matches)

-- | Queries the HTML using a CSS selector, and passes if any matched
-- element contains the given string.
--
-- Since 0.3.5
htmlAnyContain :: Query -> String -> YesodExample site mocks ()
htmlAnyContain query search = do
  matches <- htmlQuery query
  case matches of
    [] -> failure $ "Nothing matched css query: " <> query
    _ -> liftIO $ HUnit.assertBool ("None of "++T.unpack query++" contain "++search) $
          DL.any (DL.isInfixOf search) (map (TL.unpack . decodeUtf8) matches)

-- | Queries the HTML using a CSS selector, and fails if any matched
-- element contains the given string (in other words, it is the logical
-- inverse of htmlAnyContains).
--
-- Since 1.2.2
htmlNoneContain :: Query -> String -> YesodExample site mocks ()
htmlNoneContain query search = do
  matches <- htmlQuery query
  case DL.filter (DL.isInfixOf search) (map (TL.unpack . decodeUtf8) matches) of
    [] -> return ()
    found -> failure $ "Found " <> T.pack (show $ length found) <>
                " instances of " <> T.pack search <> " in " <> query <> " elements"

-- | Performs a CSS query on the last response and asserts the matched elements
-- are as many as expected.
htmlCount :: Query -> Int -> YesodExample site mocks ()
htmlCount query count = do
  matches <- fmap DL.length $ htmlQuery query
  liftIO $ flip HUnit.assertBool (matches == count)
    ("Expected "++(show count)++" elements to match "++T.unpack query++", found "++(show matches))

-- | Outputs the last response body to stderr (So it doesn't get captured by HSpec)
printBody :: YesodExample site mocks ()
printBody = withResponse $ \ SResponse { simpleBody = b } ->
  liftIO $ BSL8.hPutStrLn stderr b

-- | Performs a CSS query and print the matches to stderr.
printMatches :: Query -> YesodExample site mocks ()
printMatches query = do
  matches <- htmlQuery query
  liftIO $ hPutStrLn stderr $ show matches


-- | Perform a POST request to @url@.
--
-- ==== __Examples__
--
-- > post HomeR
post :: (Yesod site, RedirectUrl site url)
     => url
     -> YesodExample site mocks ()
post url = request $ do
  setMethod "POST"
  setUrl url

-- | Perform a POST request to @url@ with the given body.
--
-- ==== __Examples__
--
-- > postBody HomeR "foobar"
--
-- > import Data.Aeson
-- > postBody HomeR (encode $ object ["age" .= (1 :: Integer)])
postBody :: (Yesod site, RedirectUrl site url)
         => url
         -> BSL8.ByteString
         -> YesodExample site mocks ()
postBody url body = request $ do
  setMethod "POST"
  setUrl url
  setRequestBody body

-- | Perform a GET request to @url@.
--
-- ==== __Examples__
--
-- > get HomeR
--
-- > get ("http://google.com" :: Text)
get :: (Yesod site, RedirectUrl site url)
    => url
    -> YesodExample site mocks ()
get url = request $ do
    setMethod "GET"
    setUrl url

-- | Follow a redirect, if the last response was a redirect.
-- (We consider a request a redirect if the status is
-- 301, 302, 303, 307 or 308, and the Location header is set.)
--
-- ==== __Examples__
--
-- > get HomeR
-- > followRedirect
followRedirect :: Yesod site
               =>  YesodExample site mocks (Either T.Text T.Text) -- ^ 'Left' with an error message if not a redirect, 'Right' with the redirected URL if it was
followRedirect = do
  mr <- getResponse
  case mr of
   Nothing ->  return $ Left "followRedirect called, but there was no previous response, so no redirect to follow"
   Just r -> do
     if not ((H.statusCode $ simpleStatus r) `elem` [301, 302, 303, 307, 308])
       then return $ Left "followRedirect called, but previous request was not a redirect"
       else do
         case lookup "Location" (simpleHeaders r) of
          Nothing -> return $ Left "followRedirect called, but no location header set"
          Just h -> let url = TE.decodeUtf8 h in
                     get url  >> return (Right url)

-- | The general interface for performing requests. 'request' takes a 'RequestBuilder',
-- constructs a request, and executes it.
--
-- The 'RequestBuilder' allows you to build up attributes of the request, like the
-- headers, parameters, and URL of the request.
--
-- ==== __Examples__
--
-- > request $ do
-- >   addToken
-- >   byLabel "First Name" "Felipe"
-- >   setMethod "PUT"
-- >   setUrl NameR
request :: Yesod site
        => RequestBuilder site ()
        -> YesodExample site mocks ()
request reqBuilder = do
    YesodExampleData app site oldCookies mRes mocks <- ST.get

    RequestBuilderData {..} <- liftIO $ ST.execStateT reqBuilder RequestBuilderData
      { rbdPostData = MultipleItemsPostData []
      , rbdResponse = mRes
      , rbdMethod = "GET"
      , rbdSite = site
      , rbdPath = []
      , rbdGets = []
      , rbdHeaders = []
      }
    let path
            | null rbdPath = "/"
            | otherwise = TE.decodeUtf8 $ Builder.toByteString $ H.encodePathSegments rbdPath

    -- expire cookies and filter them for the current path. TODO: support max age
    currentUtc <- liftIO getCurrentTime
    let cookies = M.filter (checkCookieTime currentUtc) oldCookies
        cookiesForPath = M.filter (checkCookiePath path) cookies

    let req = case rbdPostData of
          MultipleItemsPostData x ->
            if DL.any isFile x
            then (multipart x)
            else singlepart
          BinaryPostData _ -> singlepart
          where singlepart = makeSinglepart cookiesForPath rbdPostData rbdMethod rbdHeaders path rbdGets
                multipart x = makeMultipart cookiesForPath x rbdMethod rbdHeaders path rbdGets
    -- let maker = case rbdPostData of
    --       MultipleItemsPostData x ->
    --         if DL.any isFile x
    --         then makeMultipart
    --         else makeSinglepart
    --       BinaryPostData _ -> makeSinglepart
    -- let req = maker cookiesForPath rbdPostData rbdMethod rbdHeaders path rbdGets
    response <- liftIO $ runSession (srequest req
        { simpleRequest = (simpleRequest req)
            { httpVersion = H.http11
            }
        }) app
    let newCookies = parseSetCookies $ simpleHeaders response
        cookies' = M.fromList [(Cookie.setCookieName c, c) | c <- newCookies] `M.union` cookies
    ST.put $ YesodExampleData app site cookies' (Just response) mocks
  where
    isFile (ReqFilePart _ _ _ _) = True
    isFile _ = False

    checkCookieTime t c = case Cookie.setCookieExpires c of
                              Nothing -> True
                              Just t' -> t < t'
    checkCookiePath url c =
      case Cookie.setCookiePath c of
        Nothing -> True
        Just x  -> x `BS8.isPrefixOf` TE.encodeUtf8 url

    -- For building the multi-part requests
    boundary :: String
    boundary = "*******noneedtomakethisrandom"
    separator = BS8.concat ["--", BS8.pack boundary, "\r\n"]
    makeMultipart :: M.Map a0 Cookie.SetCookie
                  -> [RequestPart]
                  -> H.Method
                  -> [H.Header]
                  -> T.Text
                  -> H.Query
                  -> SRequest
    makeMultipart cookies parts method extraHeaders urlPath urlQuery =
      SRequest simpleRequest' (simpleRequestBody' parts)
      where simpleRequestBody' x =
              BSL8.fromChunks [multiPartBody x]
            simpleRequest' = mkRequest
                             [ ("Cookie", cookieValue)
                             , ("Content-Type", contentTypeValue)]
                             method extraHeaders urlPath urlQuery
            cookieValue = Builder.toByteString $ Cookie.renderCookies cookiePairs
            cookiePairs = [ (Cookie.setCookieName c, Cookie.setCookieValue c)
                          | c <- map snd $ M.toList cookies ]
            contentTypeValue = BS8.pack $ "multipart/form-data; boundary=" ++ boundary
    multiPartBody parts =
      BS8.concat $ separator : [BS8.concat [multipartPart p, separator] | p <- parts]
    multipartPart (ReqKvPart k v) = BS8.concat
      [ "Content-Disposition: form-data; "
      , "name=\"", TE.encodeUtf8 k, "\"\r\n\r\n"
      , TE.encodeUtf8 v, "\r\n"]
    multipartPart (ReqFilePart k v bytes mime) = BS8.concat
      [ "Content-Disposition: form-data; "
      , "name=\"", TE.encodeUtf8 k, "\"; "
      , "filename=\"", BS8.pack v, "\"\r\n"
      , "Content-Type: ", TE.encodeUtf8 mime, "\r\n\r\n"
      , BS8.concat $ BSL8.toChunks bytes, "\r\n"]

    -- For building the regular non-multipart requests
    makeSinglepart :: M.Map a0 Cookie.SetCookie
                   -> RBDPostData
                   -> H.Method
                   -> [H.Header]
                   -> T.Text
                   -> H.Query
                   -> SRequest
    makeSinglepart cookies rbdPostData method extraHeaders urlPath urlQuery =
      SRequest simpleRequest' (simpleRequestBody' rbdPostData)
      where
        simpleRequest' = (mkRequest
                          ([ ("Cookie", cookieValue) ] ++ headersForPostData rbdPostData)
                          method extraHeaders urlPath urlQuery)
        simpleRequestBody' (MultipleItemsPostData x) =
          BSL8.fromChunks $ return $ TE.encodeUtf8 $ T.intercalate "&"
          $ map singlepartPart x
        simpleRequestBody' (BinaryPostData x) = x
        cookieValue = Builder.toByteString $ Cookie.renderCookies cookiePairs
        cookiePairs = [ (Cookie.setCookieName c, Cookie.setCookieValue c)
                      | c <- map snd $ M.toList cookies ]
        singlepartPart (ReqFilePart _ _ _ _) = ""
        singlepartPart (ReqKvPart k v) = T.concat [k,"=",v]

        -- If the request appears to be submitting a form (has key-value pairs) give it the form-urlencoded Content-Type.
        -- The previous behavior was to always use the form-urlencoded Content-Type https://github.com/yesodweb/yesod/issues/1063
        headersForPostData (MultipleItemsPostData []) = []
        headersForPostData (MultipleItemsPostData _ ) = [("Content-Type", "application/x-www-form-urlencoded")]
        headersForPostData (BinaryPostData _ ) = []


    -- General request making
    mkRequest headers method extraHeaders urlPath urlQuery = defaultRequest
      { requestMethod = method
      , remoteHost = Sock.SockAddrInet 1 2
      , requestHeaders = headers ++ extraHeaders
      , rawPathInfo = TE.encodeUtf8 urlPath
      , pathInfo = H.decodePathSegments $ TE.encodeUtf8 urlPath
      , rawQueryString = H.renderQuery False urlQuery
      , queryString = urlQuery
      }


type TestApp site mocks = (site, Middleware, mocks)
testApp :: site -> Middleware -> mocks -> TestApp site mocks
testApp site middleware mocks = (site, middleware, mocks)
type YSpec site mocks = Hspec.SpecWith (TestApp site mocks)

instance YesodDispatch site => Hspec.Example (ST.StateT (YesodExampleData site mocks) IO a) where
    type Arg (ST.StateT (YesodExampleData site mocks) IO a) = TestApp site mocks

    evaluateExample example params action =
        Hspec.evaluateExample
            (action $ \(site, middleware, mocks) -> do
                app <- toWaiAppPlain site
                _ <- ST.evalStateT example YesodExampleData
                    { yedApp = middleware app
                    , yedSite = site
                    , yedCookies = M.empty
                    , yedResponse = Nothing
                    , yedMocks = mocks
                    }
                return ())
            params
            ($ ())
