{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Yesod.Form.Functions
    ( -- * Running in MForm monad
      newFormIdent
    , askParams
    , askFiles
      -- * Applicative/Monadic conversion
    , formToAForm
    , aFormToForm
    , mFormToWForm
    , wFormToAForm
    , wFormToMForm
      -- * Fields to Forms
    , wreq
    , wreqMsg
    , wopt
    , mreq
    , mreqMsg
    , mopt
    , areq
    , areqMsg
    , aopt
      -- * Run a form
    , runFormPost
    , runFormPostNoToken
    , runFormGet
      -- ** Post\/Redirect\/Get
    , runFormPRG
      -- * Generate a blank form
    , generateFormPost
    , generateFormGet'
    , generateFormGet
      -- * More than one form on a handler
    , identifyForm
      -- * Rendering
    , FormRender
    , renderTable
    , renderDivs
    , renderDivsNoLabels
    , renderBootstrap
    , renderBootstrap2
      -- * Validation
    , check
    , checkBool
    , checkM
    , checkMMap
    , customErrorMessage
      -- * Utilities
    , fieldSettingsLabel
    , parseHelper
    , parseHelperGen
    , convertField
    , addClass
    , removeClass
    , lookupRawFieldInput
    ) where

import Yesod.Form.Types
import Data.Text (Text, pack)
import qualified Data.Text as T
import Control.Arrow (second)
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS (ask, get, put, runRWST, tell, evalRWST, local, mapRWST)
import Control.Monad.Trans.Writer (runWriterT, writer)
import Control.Monad (liftM, join, when)
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import qualified Data.Aeson as A
import Data.Byteable (constEqBytes)
import qualified Data.ByteString.Lazy as BL
import Text.Blaze (Markup, toMarkup)
#define Html Markup
#define toHtml toMarkup
import Yesod.Core
import Network.Wai (Request, requestMethod, rawPathInfo)
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Control.Arrow (first)

-- | Get a unique identifier.
newFormIdent :: Monad m => MForm m Text
newFormIdent = do
    i <- get
    let i' = incrInts i
    put i'
    return $ pack $ 'f' : show i'
  where
    incrInts (IntSingle i) = IntSingle $ i + 1
    incrInts (IntCons i is) = (i + 1) `IntCons` is

formToAForm :: (HandlerSite m ~ site, Monad m)
            => MForm m (FormResult a, [FieldView site])
            -> AForm m a
formToAForm form = AForm $ \(site, langs) env ints -> do
    ((a, xmls), ints', enc) <- runRWST form (env, site, langs) ints
    return (a, (++) xmls, ints', enc)

aFormToForm :: (Monad m, HandlerSite m ~ site)
            => AForm m a
            -> MForm m (FormResult a, [FieldView site] -> [FieldView site])
aFormToForm (AForm aform) = do
    ints <- get
    (env, site, langs) <- ask
    (a, xml, ints', enc) <- lift $ aform (site, langs) env ints
    put ints'
    tell enc
    return (a, xml)

askParams :: Monad m => MForm m (Maybe Env)
askParams = do
    (x, _, _) <- ask
    return $ liftM fst x

askFiles :: Monad m => MForm m (Maybe FileEnv)
askFiles = do
    (x, _, _) <- ask
    return $ liftM snd x

-- | Converts a form field into monadic form 'WForm'. This field requires a
-- value and will return 'FormFailure' if left empty.
--
-- @since 1.4.14
wreq :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
     => Field m a           -- ^ form field
     -> FieldSettings site  -- ^ settings for this field
     -> Maybe a             -- ^ optional default value
     -> WForm m (FormResult a)
wreq f fs = wreqMsg f fs MsgValueRequired

-- | Same as @wreq@ but with your own message to be rendered in case the value
-- is not provided.
--
-- This is useful when you have several required fields on the page and you
-- want to differentiate between which fields were left blank. Otherwise the
-- user sees "Value is required" multiple times, which is ambiguous.
--
-- @since 1.6.7
wreqMsg :: (RenderMessage site msg, HandlerSite m ~ site, MonadHandler m)
        => Field m a           -- ^ form field
        -> FieldSettings site  -- ^ settings for this field
        -> msg                 -- ^ message to use in case value is Nothing
        -> Maybe a             -- ^ optional default value
        -> WForm m (FormResult a)
wreqMsg f fs msg = mFormToWForm . mreqMsg f fs msg

-- | Converts a form field into monadic form 'WForm'. This field is optional,
-- i.e.  if filled in, it returns 'Just a', if left empty, it returns
-- 'Nothing'.  Arguments are the same as for 'wreq' (apart from type of default
-- value).
--
-- @since 1.4.14
wopt :: (MonadHandler m, HandlerSite m ~ site)
     => Field m a           -- ^ form field
     -> FieldSettings site  -- ^ settings for this field
     -> Maybe (Maybe a)     -- ^ optional default value
     -> WForm m (FormResult (Maybe a))
wopt f fs = mFormToWForm . mopt f fs

-- | Converts a monadic form 'WForm' into an applicative form 'AForm'.
--
-- @since 1.4.14
wFormToAForm :: MonadHandler m
             => WForm m (FormResult a)  -- ^ input form
             -> AForm m a               -- ^ output form
wFormToAForm = formToAForm . wFormToMForm

-- | Converts a monadic form 'WForm' into another monadic form 'MForm'.
--
-- @since 1.4.14
wFormToMForm :: (MonadHandler m, HandlerSite m ~ site)
             => WForm m a                      -- ^ input form
             -> MForm m (a, [FieldView site])  -- ^ output form
wFormToMForm = mapRWST (fmap group . runWriterT)
  where
    group ((a, ints, enctype), views) = ((a, views), ints, enctype)

-- | Converts a monadic form 'MForm' into another monadic form 'WForm'.
--
-- @since 1.4.14
mFormToWForm :: (MonadHandler m, HandlerSite m ~ site)
             => MForm m (a, FieldView site)  -- ^ input form
             -> WForm m a                    -- ^ output form
mFormToWForm = mapRWST $ \f -> do
  ((a, view), ints, enctype) <- lift f
  writer ((a, ints, enctype), [view])

-- | Converts a form field into monadic form. This field requires a value
-- and will return 'FormFailure' if left empty.
mreq :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
     => Field m a           -- ^ form field
     -> FieldSettings site  -- ^ settings for this field
     -> Maybe a             -- ^ optional default value
     -> MForm m (FormResult a, FieldView site)
mreq field fs mdef = mreqMsg field fs MsgValueRequired mdef

-- | Same as @mreq@ but with your own message to be rendered in case the value
-- is not provided.
--
-- This is useful when you have several required fields on the page and you
-- want to differentiate between which fields were left blank. Otherwise the
-- user sees "Value is required" multiple times, which is ambiguous.
--
-- @since 1.6.6
mreqMsg :: (RenderMessage site msg, HandlerSite m ~ site, MonadHandler m)
        => Field m a           -- ^ form field
        -> FieldSettings site  -- ^ settings for this field
        -> msg                 -- ^ Message to use in case value is Nothing
        -> Maybe a             -- ^ optional default value
        -> MForm m (FormResult a, FieldView site)
mreqMsg field fs msg mdef = mhelper field fs mdef formFailure FormSuccess True
  where formFailure m l = FormFailure [renderMessage m l msg]

-- | Converts a form field into monadic form. This field is optional, i.e.
-- if filled in, it returns 'Just a', if left empty, it returns 'Nothing'.
-- Arguments are the same as for 'mreq' (apart from type of default value).
mopt :: (site ~ HandlerSite m, MonadHandler m)
     => Field m a
     -> FieldSettings site
     -> Maybe (Maybe a)
     -> MForm m (FormResult (Maybe a), FieldView site)
mopt field fs mdef = mhelper field fs (join mdef) (const $ const $ FormSuccess Nothing) (FormSuccess . Just) False

-- | Per-request store of the raw parameter values each field was run
-- against, keyed by field name; see 'lookupRawFieldInput'.
newtype RawFieldInput = RawFieldInput (IORef (Map.Map Text [Text]))

rawFieldInputRef :: MonadHandler m => m (IORef (Map.Map Text [Text]))
rawFieldInputRef = do
    RawFieldInput ref <- cachedBy "yesod-form-raw-field-input" $
        fmap RawFieldInput $ liftIO $ newIORef Map.empty
    return ref

-- | The raw parameter values the named field was run against in this
-- request, whether from a direct submission or a Post\/Redirect\/Get
-- replay ('runFormPRG'). Empty if no form containing the field has run
-- with a submission in this request (e.g. 'generateFormPost', or an
-- 'identifyForm'-wrapped form that was not the one submitted).
--
-- This lets the view of a field rendering several inputs under one name
-- restore all of the user's raw input after a failed submission: the
-- @Either Text a@ argument a view receives can only carry the first
-- submitted value. Only fields run through 'mreq'\/'mopt' (and the
-- applicative\/widget helpers built on them) record their values here.
--
-- @since 1.7.11
lookupRawFieldInput :: MonadHandler m => Text -> m [Text]
lookupRawFieldInput name = do
    ref <- rawFieldInputRef
    Map.findWithDefault [] name `liftM` liftIO (readIORef ref)

mhelper :: (site ~ HandlerSite m, MonadHandler m)
        => Field m a
        -> FieldSettings site
        -> Maybe a
        -> (site -> [Text] -> FormResult b) -- ^ on missing
        -> (a -> FormResult b) -- ^ on success
        -> Bool -- ^ is it required?
        -> MForm m (FormResult b, FieldView site)

mhelper Field {..} FieldSettings {..} mdef onMissing onFound isReq = do
    tell fieldEnctype
    mp <- askParams
    name <- maybe newFormIdent return fsName
    theId <- lift $ maybe newIdent return fsId
    (_, site, langs) <- ask
    let mr2 = renderMessage site langs
    (res, val) <-
        case mp of
            Nothing -> return (FormMissing, maybe (Left "") Right mdef)
            Just p -> do
                mfs <- askFiles
                let mvals = fromMaybe [] $ Map.lookup name p
                    files = fromMaybe [] $ mfs >>= Map.lookup name
                lift $ do
                    ref <- rawFieldInputRef
                    liftIO $ modifyIORef' ref $ Map.insert name mvals
                emx <- lift $ fieldParse mvals files
                return $ case emx of
                    Left (SomeMessage e) -> (FormFailure [renderMessage site langs e], maybe (Left "") Left (listToMaybe mvals))
                    Right mx ->
                        case mx of
                            Nothing -> (onMissing site langs, Left "")
                            Just x -> (onFound x, Right x)
    return (res, FieldView
        { fvLabel = toHtml $ mr2 fsLabel
        , fvTooltip = fmap toHtml $ fmap mr2 fsTooltip
        , fvId = theId
        , fvInput = fieldView theId name fsAttrs val isReq
        , fvErrors =
            case res of
                FormFailure [e] -> Just $ toHtml e
                _ -> Nothing
        , fvRequired = isReq
        })

-- | Applicative equivalent of 'mreq'.
areq :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
     => Field m a           -- ^ form field
     -> FieldSettings site  -- ^ settings for this field
     -> Maybe a             -- ^ optional default value
     -> AForm m a
areq f fs = areqMsg f fs MsgValueRequired

-- | Same as @areq@ but with your own message to be rendered in case the value
-- is not provided.
--
-- This is useful when you have several required fields on the page and you
-- want to differentiate between which fields were left blank. Otherwise the
-- user sees "Value is required" multiple times, which is ambiguous.
--
-- @since 1.6.7
areqMsg :: (RenderMessage site msg, HandlerSite m ~ site, MonadHandler m)
        => Field m a           -- ^ form field
        -> FieldSettings site  -- ^ settings for this field
        -> msg                 -- ^ message to use in case value is Nothing
        -> Maybe a             -- ^ optional default value
        -> AForm m a
areqMsg f fs msg = formToAForm . liftM (second return) . mreqMsg f fs msg

-- | Applicative equivalent of 'mopt'.
aopt :: MonadHandler m
     => Field m a
     -> FieldSettings (HandlerSite m)
     -> Maybe (Maybe a)
     -> AForm m (Maybe a)
aopt a b = formToAForm . liftM (second return) . mopt a b

runFormGeneric :: Monad m
               => MForm m a
               -> HandlerSite m
               -> [Text]
               -> Maybe (Env, FileEnv)
               -> m (a, Enctype)
runFormGeneric form site langs env = evalRWST form (env, site, langs) (IntSingle 0)

-- | This function is used to both initially render a form and to later extract
-- results from it. Note that, due to CSRF protection and a few other issues,
-- forms submitted via GET and POST are slightly different. As such, be sure to
-- call the relevant function based on how the form will be submitted, /not/
-- the current request method.
--
-- For example, a common case is displaying a form on a GET request and having
-- the form submit to a POST page. In such a case, both the GET and POST
-- handlers should use 'runFormPost'.
runFormPost :: (RenderMessage (HandlerSite m) FormMessage, MonadResource m, MonadHandler m)
            => (Html -> MForm m (FormResult a, xml))
            -> m ((FormResult a, xml), Enctype)
runFormPost form = do
    env <- postEnv
    postHelper form env

postHelper  :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage)
            => (Html -> MForm m (FormResult a, xml))
            -> Maybe (Env, FileEnv)
            -> m ((FormResult a, xml), Enctype)
postHelper form env = do
    req <- getRequest
    let tokenKey = defaultCsrfParamName
    let token =
            case reqToken req of
                Nothing -> mempty
                Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]
    m <- getYesod
    langs <- languages
    ((res, xml), enctype) <- runFormGeneric (form token) m langs env
    let res' =
            case (res, env) of
                (_, Nothing) -> FormMissing
                (FormSuccess{}, Just (params, _))
                    | not (Map.lookup tokenKey params === reqToken req) ->
                        FormFailure [renderMessage m langs MsgCsrfWarning]
                _ -> res
            -- It's important to use constant-time comparison (constEqBytes) in order to avoid timing attacks.
            where (Just [t1]) === (Just t2) = TE.encodeUtf8 t1 `constEqBytes` TE.encodeUtf8 t2
                  Nothing     === Nothing   = True
                  _           === _         = False
    return ((res', xml), enctype)

-- | Similar to 'runFormPost', except it always ignores the currently available
-- environment. This is necessary in cases like a wizard UI, where a single
-- page will both receive and incoming form and produce a new, blank form. For
-- general usage, you can stick with @runFormPost@.
generateFormPost
    :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
    => (Html -> MForm m (FormResult a, xml))
    -> m (xml, Enctype)
generateFormPost form = first snd `liftM` postHelper form Nothing

-- | Run a form for the Post\/Redirect\/Get pattern, so that invalid input
-- and its validation errors survive a redirect.
--
-- Use this in /both/ the GET and POST handlers of a route, exactly as you
-- would 'runFormPost':
--
-- * On a POST request this behaves exactly like 'runFormPost' (including
--   CSRF protection). Additionally, on 'FormFailure' the submitted
--   parameters are stashed in the user session so that your handler can
--   simply 'redirect' back to the GET route instead of re-rendering. On
--   'FormSuccess' any previously stashed parameters for this route are
--   cleared. On 'FormMissing' (another 'identifyForm'-wrapped form on the
--   page was the one submitted) the stash is left untouched.
--
-- * On a GET request, if stashed parameters exist for the current path,
--   they are removed from the session and the form is run against them:
--   the fields re-render with the user's input and validation errors, just
--   as a non-redirecting handler would have rendered them. A fresh CSRF
--   token is rendered into the form (the stashed token is discarded, and
--   no token check is performed on a replay). If nothing is stashed, this
--   behaves like 'generateFormPost' (the result is 'FormMissing').
--
-- > getRegisterR :: Handler Html
-- > getRegisterR = do
-- >     ((_res, widget), enctype) <- runFormPRG personForm
-- >     defaultLayout
-- >         [whamlet|
-- >             <form method=post action=@{RegisterR} enctype=#{enctype}>
-- >                 ^{widget}
-- >         |]
-- >
-- > postRegisterR :: Handler Html
-- > postRegisterR = do
-- >     ((res, _widget), _enctype) <- runFormPRG personForm
-- >     case res of
-- >         FormSuccess person -> do
-- >             -- insert into the database, etc.
-- >             setMessage "Registered!"
-- >             redirect RegisterR
-- >         _ -> redirect RegisterR
--
-- Notes and caveats:
--
-- * The stash is keyed by request path, so the GET and POST handlers must
--   share a route (the common Yesod pattern). Form pages on different
--   routes do not interfere with each other; two tabs on the /same/ path
--   share one stash (the last failed submission wins).
--
-- * Multiple forms on one page work when wrapped with 'identifyForm': on
--   replay, only the form that was actually submitted re-renders with
--   values and errors; the others return 'FormMissing' and render blank.
--   (The stash is popped once per request and shared between calls via
--   the per-request cache, so every form in the handler sees it.)
--
-- * Uploaded files are not stashed; file fields re-render empty after the
--   redirect.
--
-- * The stashed parameters live in the user session. With the default
--   client-session backend the whole session must fit in a cookie
--   (roughly 4KB), so this is unsuitable for very large forms. If the
--   stash fails to decode, it is discarded and a blank form is generated.
--
-- * If the application has no session backend, stashing is a no-op and
--   this degrades to 'runFormPost'\/'generateFormPost' semantics (input
--   will not survive the redirect).
--
-- * If your POST handler re-renders on failure instead of redirecting, do
--   not use this function: the stash would be consumed by an unrelated
--   later GET of the same path.
--
-- * On a replay the form may even produce 'FormSuccess' (e.g. a 'checkM'
--   validation whose outcome depends on external state); the result is
--   returned as-is. GET handlers normally use only the widget.
--
-- * A HEAD request reads the stash without consuming it, so that
--   middleware such as @autoHead@ does not eat the replay meant for the
--   subsequent GET.
--
-- @since 1.7.10
runFormPRG
    :: (RenderMessage (HandlerSite m) FormMessage, MonadResource m, MonadHandler m)
    => (Html -> MForm m (FormResult a, xml))
    -> m ((FormResult a, xml), Enctype)
runFormPRG form = do
    req <- getRequest
    let method = requestMethod $ reqWaiRequest req
        key = prgSessionKey $ reqWaiRequest req
    if method == "GET" || method == "HEAD"
        then do
            -- The per-request cache lets several calls in one handler
            -- (multiple forms on a page) all see the stash, even though it
            -- is deleted from the session on first read.
            PRGStash menv <- cachedBy (TE.encodeUtf8 key) $ do
                mbs <- lookupSessionBS key
                when (method /= "HEAD") $ deleteSession key
                return $ PRGStash $ mbs >>= A.decodeStrict
            case menv of
                Nothing -> postHelper form Nothing
                Just env -> replayHelper form env
        else do
            env <- postEnv
            res@((formRes, _), _) <- postHelper form env
            case (formRes, env) of
                (FormSuccess{}, _) -> deleteSession key
                (FormFailure{}, Just (params, _)) -> setSessionBS key $
                    BL.toStrict $ A.encode $
                    Map.delete defaultCsrfParamName params
                -- FormMissing means another identified form on this page
                -- was the one submitted; leave its stash alone.
                _ -> return ()
            return res

-- | Per-request cache wrapper for the Post\/Redirect\/Get stash; see
-- 'runFormPRG'.
newtype PRGStash = PRGStash (Maybe Env)

-- | Like 'postHelper', but runs the form against a supplied 'Env' (with an
-- empty 'FileEnv') instead of the current request body. A fresh CSRF token
-- fragment is rendered into the form, and no token check is performed: the
-- replayed submission was already checked by 'postHelper' when it was
-- stashed, and this run is a re-render, not a submission.
replayHelper :: MonadHandler m
             => (Html -> MForm m a)
             -> Env
             -> m (a, Enctype)
replayHelper form env = do
    req <- getRequest
    let token =
            case reqToken req of
                Nothing -> mempty
                Just n -> [shamlet|<input type=hidden name=#{defaultCsrfParamName} value=#{n}>|]
    m <- getYesod
    langs <- languages
    runFormGeneric (form token) m langs (Just (env, Map.empty))

-- | Session key for the Post\/Redirect\/Get stash, namespaced by request
-- path so that form pages on different routes do not interfere.
prgSessionKey :: Request -> Text
prgSessionKey req = "_PRG:" <> TE.decodeUtf8With TEE.lenientDecode (rawPathInfo req)

postEnv :: MonadHandler m => m (Maybe (Env, FileEnv))
postEnv = do
    req <- getRequest
    if requestMethod (reqWaiRequest req) == "GET"
        then return Nothing
        else do
            (p, f) <- runRequestBody
            let p' = Map.unionsWith (++) $ map (\(x, y) -> Map.singleton x [y]) p
            return $ Just (p', Map.unionsWith (++) $ map (\(k, v) -> Map.singleton k [v]) f)

runFormPostNoToken :: MonadHandler m
                   => (Html -> MForm m a)
                   -> m (a, Enctype)
runFormPostNoToken form = do
    langs <- languages
    m <- getYesod
    env <- postEnv
    runFormGeneric (form mempty) m langs env

runFormGet :: MonadHandler m
           => (Html -> MForm m a)
           -> m (a, Enctype)
runFormGet form = do
    gets <- liftM reqGetParams getRequest
    let env =
            case lookup getKey gets of
                Nothing -> Nothing
                Just _ -> Just (Map.unionsWith (++) $ map (\(x, y) -> Map.singleton x [y]) gets, Map.empty)
    getHelper form env

{- FIXME: generateFormGet' "Will be renamed to generateFormGet in next version of Yesod" -}
-- |
--
-- Since 1.3.11
generateFormGet'
    :: MonadHandler m
    => (Html -> MForm m (FormResult a, xml))
    -> m (xml, Enctype)
generateFormGet' form = first snd `liftM` getHelper form Nothing

{-# DEPRECATED generateFormGet "Will require RenderMessage in next version of Yesod" #-}
generateFormGet :: MonadHandler m
                => (Html -> MForm m a)
                -> m (a, Enctype)
generateFormGet form = getHelper form Nothing

getKey :: Text
getKey = "_hasdata"

getHelper :: MonadHandler m
          => (Html -> MForm m a)
          -> Maybe (Env, FileEnv)
          -> m (a, Enctype)
getHelper form env = do
    let fragment = [shamlet|<input type=hidden name=#{getKey}>|]
    langs <- languages
    m <- getYesod
    runFormGeneric (form fragment) m langs env


-- | Creates a hidden field on the form that identifies it.  This
-- identification is then used to distinguish between /missing/
-- and /wrong/ form data when a single handler contains more than
-- one form.
--
-- For instance, if you have the following code on your handler:
--
-- > ((fooRes, fooWidget), fooEnctype) <- runFormPost fooForm
-- > ((barRes, barWidget), barEnctype) <- runFormPost barForm
--
-- Then replace it with
--
-- > ((fooRes, fooWidget), fooEnctype) <- runFormPost $ identifyForm "foo" fooForm
-- > ((barRes, barWidget), barEnctype) <- runFormPost $ identifyForm "bar" barForm
--
-- Note that it's your responsibility to ensure that the
-- identification strings are unique (using the same one twice on a
-- single handler will not generate any errors).  This allows you
-- to create a variable number of forms and still have them work
-- even if their number or order change between the HTML
-- generation and the form submission.
identifyForm
  :: Monad m
  => Text -- ^ Form identification string.
  -> (Html -> MForm m (FormResult a, WidgetFor (HandlerSite m) ()))
  -> (Html -> MForm m (FormResult a, WidgetFor (HandlerSite m) ()))
identifyForm identVal form = \fragment -> do
    -- Create hidden <input>.
    let fragment' =
          [shamlet|
            <input type=hidden name=#{identifyFormKey} value=identify-#{identVal}>
            #{fragment}
          |]

    -- Check if we got its value back.
    mp <- askParams
    let missing = (mp >>= Map.lookup identifyFormKey) /= Just ["identify-" <> identVal]

    -- Run the form proper (with our hidden <input>).  If the
    -- data is missing, then do not provide any params to the
    -- form, which will turn its result into FormMissing.  Also,
    -- doing this avoids having lots of fields with red errors.
    let eraseParams | missing   = local (\(_, h, l) -> (Nothing, h, l))
                    | otherwise = id
    ( res', w) <- eraseParams (form fragment')

    -- Empty forms now properly return FormMissing. [#1072](https://github.com/yesodweb/yesod/issues/1072)
    let res = if missing then FormMissing else res'
    return ( res, w)

identifyFormKey :: Text
identifyFormKey = "_formid"


type FormRender m a =
       AForm m a
    -> Html
    -> MForm m (FormResult a, WidgetFor (HandlerSite m) ())

renderTable, renderDivs, renderDivsNoLabels :: Monad m => FormRender m a
-- | Render a form into a series of tr tags. Note that, in order to allow
-- you to add extra rows to the table, this function does /not/ wrap up
-- the resulting HTML in a table tag; you must do that yourself.
renderTable aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
    let widget = [whamlet|
$newline never
$if null views
    \#{fragment}
$forall (isFirst, view) <- addIsFirst views
    <tr :fvRequired view:.required :not $ fvRequired view:.optional>
        <td>
            $if isFirst
                \#{fragment}
            <label for=#{fvId view}>#{fvLabel view}
            $maybe tt <- fvTooltip view
                <div .tooltip>#{tt}
        <td>^{fvInput view}
        $maybe err <- fvErrors view
            <td .errors>#{err}
|]
    return (res, widget)
  where
    addIsFirst [] = []
    addIsFirst (x:y) = (True, x) : map (False, ) y

-- | render a field inside a div
renderDivs = renderDivsMaybeLabels True

-- | render a field inside a div, not displaying any label
renderDivsNoLabels = renderDivsMaybeLabels False

renderDivsMaybeLabels :: Monad m => Bool -> FormRender m a
renderDivsMaybeLabels withLabels aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
    let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <div :fvRequired view:.required :not $ fvRequired view:.optional>
        $if withLabels
                <label for=#{fvId view}>#{fvLabel view}
        $maybe tt <- fvTooltip view
            <div .tooltip>#{tt}
        ^{fvInput view}
        $maybe err <- fvErrors view
            <div .errors>#{err}
|]
    return (res, widget)

-- | Render a form using Bootstrap v2-friendly shamlet syntax.
-- If you're using Bootstrap v3, then you should use the
-- functions from module "Yesod.Form.Bootstrap3".
--
-- Sample Hamlet:
--
-- >  <form .form-horizontal method=post action=@{ActionR} enctype=#{formEnctype}>
-- >    <fieldset>
-- >      <legend>_{MsgLegend}
-- >      $case result
-- >        $of FormFailure reasons
-- >          $forall reason <- reasons
-- >            <div .alert .alert-error>#{reason}
-- >        $of _
-- >      ^{formWidget}
-- >      <div .form-actions>
-- >        <input .btn .primary type=submit value=_{MsgSubmit}>
--
-- Since 1.3.14
renderBootstrap2 :: Monad m => FormRender m a
renderBootstrap2 aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
    let widget = [whamlet|
                $newline never
                \#{fragment}
                $forall view <- views
                    <div .control-group .clearfix :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.error>
                        <label .control-label for=#{fvId view}>#{fvLabel view}
                        <div .controls .input>
                            ^{fvInput view}
                            $maybe tt <- fvTooltip view
                                <span .help-block>#{tt}
                            $maybe err <- fvErrors view
                                <span .help-block>#{err}
                |]
    return (res, widget)

-- | Deprecated synonym for 'renderBootstrap2'.
renderBootstrap :: Monad m => FormRender m a
renderBootstrap = renderBootstrap2
{-# DEPRECATED renderBootstrap "Please use the Yesod.Form.Bootstrap3 module." #-}

check :: (Monad m, RenderMessage (HandlerSite m) msg)
      => (a -> Either msg a)
      -> Field m a
      -> Field m a
check f = checkM $ return . f

-- | Return the given error message if the predicate is false.
checkBool :: (Monad m, RenderMessage (HandlerSite m) msg)
          => (a -> Bool) -> msg -> Field m a -> Field m a
checkBool b s = check $ \x -> if b x then Right x else Left s

checkM :: (Monad m, RenderMessage (HandlerSite m) msg)
       => (a -> m (Either msg a))
       -> Field m a
       -> Field m a
checkM f = checkMMap f id

-- | Same as 'checkM', but modifies the datatype.
--
-- In order to make this work, you must provide a function to convert back from
-- the new datatype to the old one (the second argument to this function).
--
-- Since 1.1.2
checkMMap :: (Monad m, RenderMessage (HandlerSite m) msg)
          => (a -> m (Either msg b))
          -> (b -> a)
          -> Field m a
          -> Field m b
checkMMap f inv field = field
    { fieldParse = \ts fs -> do
        e1 <- fieldParse field ts fs
        case e1 of
            Left msg -> return $ Left msg
            Right Nothing -> return $ Right Nothing
            Right (Just a) -> liftM (either (Left . SomeMessage) (Right . Just)) $ f a
    , fieldView = \i n a eres req -> fieldView field i n a (fmap inv eres) req
    }

-- | Allows you to overwrite the error message on parse error.
customErrorMessage :: Monad m => SomeMessage (HandlerSite m) -> Field m a -> Field m a
customErrorMessage msg field = field
    { fieldParse = \ts fs ->
        liftM (either (const $ Left msg) Right)
      $ fieldParse field ts fs
    }

-- | Generate a 'FieldSettings' from the given label.
fieldSettingsLabel :: RenderMessage site msg => msg -> FieldSettings site
fieldSettingsLabel msg = FieldSettings (SomeMessage msg) Nothing Nothing Nothing []

-- | A helper function for creating custom fields.
--
-- This is intended to help with the common case where a single input value is
-- required, such as when parsing a text field.
--
-- Since 1.1
parseHelper :: (Monad m, RenderMessage site FormMessage)
            => (Text -> Either FormMessage a)
            -> [Text] -> [FileInfo] -> m (Either (SomeMessage site) (Maybe a))
parseHelper = parseHelperGen

-- | A generalized version of 'parseHelper', allowing any type for the message
-- indicating a bad parse.
--
-- Since 1.3.6
parseHelperGen :: (Monad m, RenderMessage site msg)
               => (Text -> Either msg a)
               -> [Text] -> [FileInfo] -> m (Either (SomeMessage site) (Maybe a))
parseHelperGen _ [] _ = return $ Right Nothing
parseHelperGen _ ("":_) _ = return $ Right Nothing
parseHelperGen f (x:_) _ = return $ either (Left . SomeMessage) (Right . Just) $ f x

-- | Since a 'Field' cannot be a 'Functor', it is not obvious how to "reuse" a Field
-- on a @newtype@ or otherwise equivalent type. This function allows you to convert
-- a @Field m a@ to a @Field m b@ assuming you provide a bidirectional
-- conversion between the two, through the first two functions.
--
-- A simple example:
--
-- > import Data.Monoid
-- > sumField :: (Functor m, Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m (Sum Int)
-- > sumField = convertField Sum getSum intField
--
-- Another example, not using a newtype, but instead creating a Lazy Text field:
--
-- > import qualified Data.Text.Lazy as TL
-- > TextField :: (Functor m, Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m TL.Text
-- > lazyTextField = convertField TL.fromStrict TL.toStrict textField
--
-- Since 1.3.16
convertField :: (Functor m)
             => (a -> b) -> (b -> a)
             -> Field m a -> Field m b
convertField to from (Field fParse fView fEnctype) = let
  fParse' ts = fmap (fmap (fmap to)) . fParse ts
  fView' ti tn at ei = fView ti tn at (fmap from ei)
  in Field fParse' fView' fEnctype

-- | Removes a CSS class from the 'fsAttrs' in a 'FieldSettings'.
--
-- ==== __Examples__
--
-- >>> removeClass "form-control" [("class","form-control login-form"),("id","home-login")]
-- [("class","  login-form"),("id","home-login")]
--
-- @since 1.6.2
removeClass :: Text -- ^ The class to remove
            -> [(Text, Text)] -- ^ List of existing 'fsAttrs'
            -> [(Text, Text)]
removeClass _     []                    = []
removeClass klass (("class", old):rest) = ("class", T.replace klass " " old) : rest
removeClass klass (other         :rest) = other : removeClass klass rest

-- | Adds a CSS class to the 'fsAttrs' in a 'FieldSettings'.
--
-- ==== __Examples__
--
-- >>> addClass "login-form" [("class", "form-control"), ("id", "home-login")]
-- [("class","form-control login-form"),("id","home-login")]
--
-- @since 1.6.2
addClass :: Text -- ^ The class to add
         -> [(Text, Text)] -- ^ List of existing 'fsAttrs'
         -> [(Text, Text)]
addClass klass []                    = [("class", klass)]
addClass klass (("class", old):rest) = ("class", T.concat [old, " ", klass]) : rest
addClass klass (other         :rest) = other : addClass klass rest
