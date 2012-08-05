{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Yesod.Test is a pragmatic framework for testing web applications built
using wai and persistent.

By pragmatic I may also mean 'dirty'. It's main goal is to encourage integration
and system testing of web applications by making everything /easy to test/. 

Your tests are like browser sessions that keep track of cookies and the last
visited page. You can perform assertions on the content of HTML responses,
using css selectors to explore the document more easily.

You can also easily build requests using forms present in the current page.
This is very useful for testing web applications built in yesod for example,
were your forms may have field names generated by the framework or a randomly
generated '_nonce' field.

Your database is also directly available so you can use runDBRunner to set up
backend pre-conditions, or to assert that your session is having the desired effect.

-}

module Yesod.Test (
  -- * Declaring and running your test suite
  runTests, describe, it, Specs, OneSpec,

  -- * Making requests
  -- | To make a request you need to point to an url and pass in some parameters.
  --
  -- To build your parameters you will use the RequestBuilder monad that lets you
  -- add values, add files, lookup fields by label and find the current
  -- nonce value and add it to your request too.
  -- 
  post, post_, get, get_, doRequest,
  byName, fileByName,

  -- | Yesod cat auto generate field ids, so you are never sure what
  -- the argument name should be for each one of your args when constructing
  -- your requests. What you do know is the /label/ of the field.
  -- These functions let you add parameters to your request based
  -- on currently displayed label names.
  byLabel, fileByLabel,

  -- | Does the current form have a _nonce? Use any of these to add it to your
  -- request parameters.
  addNonce, addNonce_,

  -- * Running database queries
  runDBRunner,

  -- * Assertions
  assertEqual, assertHeader, assertNoHeader, statusIs, bodyEquals, bodyContains,
  htmlAllContain, htmlCount,

  -- * Utils for debugging tests
  printBody, printMatches,

  -- * Utils for building your own assertions
  -- | Please consider generalizing and contributing the assertions you write.
  htmlQuery, parseHTML, withResponse

)

where

import qualified Test.Hspec.Core as Core
import qualified Test.Hspec.Runner as Runner
import qualified Data.List as DL
import qualified Data.Maybe as DY
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Test.HUnit as HUnit
import qualified Test.Hspec.HUnit ()
import qualified Network.HTTP.Types as H
import qualified Network.Socket.Internal as Sock
import Data.CaseInsensitive (CI)
import Network.Wai
import Network.Wai.Test hiding (assertHeader, assertNoHeader)
import qualified Control.Monad.Trans.State as ST
import Control.Monad.IO.Class
import System.IO
import Yesod.Test.TransversingCSS
import Data.Monoid (mappend)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as C
import qualified Text.HTML.DOM as HD
import Data.Conduit.Pool (Pool)
import Control.Monad.Trans.Control (MonadBaseControl)

-- | The state used in 'describe' to build a list of specs
data SpecsData conn = SpecsData Application (Pool conn) [Core.Spec]

-- | The specs state monad is where 'describe' runs.
type Specs conn = ST.StateT (SpecsData conn) IO ()

-- | The state used in a single test case defined using 'it'
data OneSpecData conn = OneSpecData Application (Pool conn) CookieValue (Maybe SResponse)

-- | The OneSpec state monad is where 'it' runs.
type OneSpec conn = ST.StateT (OneSpecData conn) IO

data RequestBuilderData = RequestBuilderData [RequestPart] (Maybe SResponse)

-- | Request parts let us discern regular key/values from files sent in the request.
data RequestPart
  = ReqPlainPart T.Text T.Text
  | ReqFilePart T.Text FilePath BSL8.ByteString T.Text

-- | The RequestBuilder state monad constructs an url encoded string of arguments
-- to send with your requests. Some of the functions that run on it use the current
-- response to analize the forms that the server is expecting to receive.
type RequestBuilder = ST.StateT RequestBuilderData IO

-- | Both the OneSpec and RequestBuilder monads hold a response that can be analized,
-- by making them instances of this class we can have general methods that work on
-- the last received response.
class HoldsResponse a where
  readResponse :: a -> Maybe SResponse
instance HoldsResponse (OneSpecData conn) where
  readResponse (OneSpecData _ _ _ x) = x
instance HoldsResponse RequestBuilderData where
  readResponse (RequestBuilderData _ x) = x
  
type CookieValue = ByteString

-- | Runs your test suite, using you wai 'Application' and 'ConnectionPool' for performing 
-- the database queries in your tests.
--
-- You application may already have your connection pool but you need to pass another one
-- separately here.
-- 
-- Look at the examples directory on this package to get an idea of the (small) amount of
-- boilerplate code you'll need to write before calling this.
runTests :: Application -> Pool conn -> Specs conn -> IO ()
runTests app connection specsDef = do
  (SpecsData _ _ specs) <- ST.execStateT specsDef (SpecsData app connection [])
  Runner.hspec specs

-- | Start describing a Tests suite keeping cookies and a reference to the tested 'Application'
-- and 'ConnectionPool'
describe :: String -> Specs conn -> Specs conn
describe label action = do
  sData <- ST.get
  SpecsData app conn specs <- liftIO $ ST.execStateT action sData
  ST.put $ SpecsData app conn [Core.describe label specs]

-- | Describe a single test that keeps cookies, and a reference to the last response.
it :: String -> OneSpec conn () -> Specs conn
it label action = do
  SpecsData app conn specs <- ST.get
  let spec = Core.it label $ do
        _ <- ST.execStateT action $ OneSpecData app conn "" Nothing
        return ()
  ST.put $ SpecsData app conn $ spec : specs

-- Performs a given action using the last response. Use this to create
-- response-level assertions
withResponse :: HoldsResponse a => (SResponse -> ST.StateT a IO b) -> ST.StateT a IO b
withResponse f = maybe err f =<< fmap readResponse ST.get
 where err = failure "There was no response, you should make a request"

-- | Use HXT to parse a value from an html tag.
-- Check for usage examples in this module's source.
parseHTML :: Html -> Cursor
parseHTML html = fromDocument $ HD.parseLBS html

-- | Query the last response using css selectors, returns a list of matched fragments
htmlQuery :: HoldsResponse a => Query -> ST.StateT a IO [Html]
htmlQuery query = withResponse $ \ res ->
  case findBySelector (simpleBody res) query of
    Left err -> failure $ query <> " did not parse: " <> T.pack (show err)
    Right matches -> return $ map (encodeUtf8 . TL.pack) matches

-- | Asserts that the two given values are equal.
assertEqual :: (Eq a) => String -> a -> a -> OneSpec conn ()
assertEqual msg a b = liftIO $ HUnit.assertBool msg (a == b)

-- | Assert the last response status is as expected.
statusIs :: HoldsResponse a => Int -> ST.StateT a IO ()
statusIs number = withResponse $ \ SResponse { simpleStatus = s } ->
  liftIO $ flip HUnit.assertBool (H.statusCode s == number) $ concat
    [ "Expected status was ", show number
    , " but received status was ", show $ H.statusCode s
    ]

-- | Assert the given header key/value pair was returned.
assertHeader :: HoldsResponse a => CI BS8.ByteString -> BS8.ByteString -> ST.StateT a IO ()
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
assertNoHeader :: HoldsResponse a => CI BS8.ByteString -> ST.StateT a IO ()
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
bodyEquals :: HoldsResponse a => String -> ST.StateT a IO ()
bodyEquals text = withResponse $ \ res ->
  liftIO $ HUnit.assertBool ("Expected body to equal " ++ text) $
    (simpleBody res) == BSL8.pack text

-- | Assert the last response has the given text. The check is performed using the response
-- body in full text form.
bodyContains :: HoldsResponse a => String -> ST.StateT a IO ()
bodyContains text = withResponse $ \ res ->
  liftIO $ HUnit.assertBool ("Expected body to contain " ++ text) $
    (simpleBody res) `contains` text

contains :: BSL8.ByteString -> String -> Bool
contains a b = DL.isInfixOf b (BSL8.unpack a)

-- | Queries the html using a css selector, and all matched elements must contain
-- the given string.
htmlAllContain :: HoldsResponse a => Query -> String -> ST.StateT a IO ()
htmlAllContain query search = do
  matches <- htmlQuery query
  case matches of
    [] -> failure $ "Nothing matched css query: " <> query
    _ -> liftIO $ HUnit.assertBool ("Not all "++T.unpack query++" contain "++search) $
          DL.all (DL.isInfixOf search) (map (TL.unpack . decodeUtf8) matches)

-- | Performs a css query on the last response and asserts the matched elements
-- are as many as expected.
htmlCount :: HoldsResponse a => Query -> Int -> ST.StateT a IO ()
htmlCount query count = do
  matches <- fmap DL.length $ htmlQuery query
  liftIO $ flip HUnit.assertBool (matches == count)
    ("Expected "++(show count)++" elements to match "++T.unpack query++", found "++(show matches))

-- | Outputs the last response body to stderr (So it doesn't get captured by HSpec)
printBody :: HoldsResponse a => ST.StateT a IO ()
printBody = withResponse $ \ SResponse { simpleBody = b } -> 
  liftIO $ hPutStrLn stderr $ BSL8.unpack b

-- | Performs a CSS query and print the matches to stderr.
printMatches :: HoldsResponse a => Query -> ST.StateT a IO ()
printMatches query = do
  matches <- htmlQuery query
  liftIO $ hPutStrLn stderr $ show matches

-- | Add a parameter with the given name and value.
byName :: T.Text -> T.Text -> RequestBuilder ()
byName name value = do
  RequestBuilderData parts r <- ST.get
  ST.put $ RequestBuilderData ((ReqPlainPart name value):parts) r

-- | Add a file to be posted with the current request
--
-- Adding a file will automatically change your request content-type to be multipart/form-data
fileByName :: T.Text -> FilePath -> T.Text -> RequestBuilder ()
fileByName name path mimetype = do
  RequestBuilderData parts r <- ST.get
  contents <- liftIO $ BSL8.readFile path
  ST.put $ RequestBuilderData ((ReqFilePart name path contents mimetype):parts) r

-- This looks up the name of a field based on the contents of the label pointing to it.
nameFromLabel :: T.Text -> RequestBuilder T.Text
nameFromLabel label = withResponse $ \ res -> do
  let
    body = simpleBody res
    mfor = parseHTML body
                $// C.element "label"
                >=> contentContains label
                >=> attribute "for"

    contentContains x c
        | x `T.isInfixOf` T.concat (c $// content) = [c]
        | otherwise = []

  case mfor of
    for:[] -> do
      let mname = parseHTML body
                    $// attributeIs "id" for
                    >=> attribute "name"
      case mname of
        "":_ -> failure $ T.concat
            [ "Label "
            , label
            , " resolved to id "
            , for
            , " which was not found. "
            ]
        name:_ -> return name
        _ -> failure $ "More than one input with id " <> for
    [] -> failure $ "No label contained: " <> label
    _ -> failure $ "More than one label contained " <> label

(<>) :: T.Text -> T.Text -> T.Text
(<>) = T.append

byLabel :: T.Text -> T.Text -> RequestBuilder ()
byLabel label value = do
  name <- nameFromLabel label
  byName name value

fileByLabel :: T.Text -> FilePath -> T.Text -> RequestBuilder ()
fileByLabel label path mime = do
  name <- nameFromLabel label
  fileByName name path mime

-- | Lookup a _nonce form field and add it's value to the params. 
-- Receives a CSS selector that should resolve to the form element containing the nonce.
addNonce_ :: Query -> RequestBuilder ()
addNonce_ scope = do
  matches <- htmlQuery $ scope `mappend` "input[name=_token][type=hidden][value]"
  case matches of
    [] -> failure $ "No nonce found in the current page"
    element:[] -> byName "_token" $ head $ attribute "value" $ parseHTML element
    _ -> failure $ "More than one nonce found in the page"

-- | For responses that display a single form, just lookup the only nonce available.
addNonce :: RequestBuilder ()
addNonce = addNonce_ ""

-- | Perform a POST request to url, using params
post :: BS8.ByteString -> RequestBuilder () -> OneSpec conn ()
post url paramsBuild = do
  doRequest "POST" url paramsBuild

-- | Perform a POST request without params
post_ :: BS8.ByteString -> OneSpec conn ()
post_ = flip post $ return ()
 
-- | Perform a GET request to url, using params
get :: BS8.ByteString -> RequestBuilder () -> OneSpec conn ()
get url paramsBuild = doRequest "GET" url paramsBuild

-- | Perform a GET request without params
get_ :: BS8.ByteString -> OneSpec conn ()
get_ = flip get $ return ()

-- | General interface to performing requests, letting you specify the request method and extra headers.
doRequest :: H.Method -> BS8.ByteString -> RequestBuilder a -> OneSpec conn ()
doRequest method url paramsBuild = do
  OneSpecData app conn cookie mRes <- ST.get
  RequestBuilderData parts _ <- liftIO $ ST.execStateT paramsBuild $ RequestBuilderData [] mRes
  let req = if DL.any isFile parts
          then makeMultipart cookie parts
          else makeSinglepart cookie parts

  response <- liftIO $ runSession (srequest req) app
  let cookie' = DY.fromMaybe cookie $ fmap snd $ DL.find (("Set-Cookie"==) . fst) $ simpleHeaders response
  ST.put $ OneSpecData app conn cookie' (Just response)
 where
  isFile (ReqFilePart _ _ _ _) = True
  isFile _ = False

  -- For building the multi-part requests
  boundary :: String
  boundary = "*******noneedtomakethisrandom"
  separator = BS8.concat ["--", BS8.pack boundary, "\r\n"]
  makeMultipart cookie parts =
    flip SRequest (BSL8.fromChunks [multiPartBody parts]) $ mkRequest
      [ ("Cookie", cookie)
      , ("Content-Type", BS8.pack $ "multipart/form-data; boundary=" ++ boundary)]
  multiPartBody parts =
    BS8.concat $ separator : [BS8.concat [multipartPart p, separator] | p <- parts]
  multipartPart (ReqPlainPart k v) = BS8.concat
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
  makeSinglepart cookie parts = SRequest (mkRequest 
    [("Cookie",cookie), ("Content-Type", "application/x-www-form-urlencoded")]) $
    BSL8.fromChunks $ return $ TE.encodeUtf8 $ T.intercalate "&" $ map singlepartPart parts

  singlepartPart (ReqFilePart _ _ _ _) = ""
  singlepartPart (ReqPlainPart k v) = T.concat [k,"=",v]

  -- General request making 
  mkRequest headers = defaultRequest
    { requestMethod = method
    , remoteHost = Sock.SockAddrInet 1 2
    , requestHeaders = headers
    , rawPathInfo = url
    , pathInfo = DL.filter (/="") $ T.split (== '/') $ TE.decodeUtf8 url
    }
  
-- | Run a persistent db query. For asserting on the results of performed actions
-- or setting up pre-conditions. At the moment this part is still very raw.
--
-- It is intended that you parametize the first argument of this function for your backend
-- runDB = runDBRunnder SqlPersist
runDBRunner :: (MonadBaseControl IO m, MonadIO m) => (poolrunner m a -> Pool conn -> IO a) -> poolrunner m a -> OneSpec conn a
runDBRunner poolRunner query = do
  OneSpecData _ pool _ _ <- ST.get
  liftIO $ poolRunner query pool

-- Yes, just a shortcut
failure :: (MonadIO a) => T.Text -> a b
failure reason = (liftIO $ HUnit.assertFailure $ T.unpack reason) >> error ""
