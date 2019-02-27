{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
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
    , wopt
    , mreq
    , mopt
    , areq
    , aopt
      -- * Run a form
    , runFormPost
    , runFormPostNoToken
    , runFormGet
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
    ) where

import RIO hiding (ask, local)
import Yesod.Form.Types
import Yesod.Core.Types (liftHandler)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Control.Arrow (second)
import Control.Monad (liftM, join)
import Data.Byteable (constEqBytes)
import Text.Blaze (Markup, toMarkup)
#define Html Markup
#define toHtml toMarkup
import Yesod.Core
import Network.Wai (requestMethod)
import Data.Monoid (mempty, (<>))
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE
import Control.Arrow (first)

get :: MForm site Ints
get = view (to mfdInts) >>= readIORef

put :: Ints -> MForm site ()
put ints = view (to mfdInts) >>= (`writeIORef` ints)

tell :: Enctype -> MForm site ()
tell ec = view (to mfdEnctype) >>= (`writeIORef` ec)

local
  :: ( Maybe (Env, FileEnv)
    -> Maybe (Env, FileEnv)
     )
  -> MForm site a
  -> MForm site a
local f inner = do
  mfd <- view id
  let mfd' = mfd { mfdParams = f $ mfdParams mfd }
  runRIO mfd' inner

-- | Get a unique identifier.
newFormIdent :: MForm site Text
newFormIdent = do
    i <- get
    let i' = incrInts i
    put i'
    return $ pack $ 'f' : show i'
  where
    incrInts (IntSingle i) = IntSingle $ i + 1
    incrInts (IntCons i is) = (i + 1) `IntCons` is

formToAForm :: MForm site (FormResult a, [FieldView site]) -> AForm site a
formToAForm mform = AForm $ do
  WFormData viewsDeque mfd <- view id
  (a, views) <- runRIO mfd mform
  for_ views $ pushBackDeque viewsDeque
  pure a

aFormToForm :: AForm site a
            -> MForm site (FormResult a, [FieldView site] -> [FieldView site])
aFormToForm (AForm wform) = do
    (res, views) <- wFormToMForm wform
    pure (res, (views++))

askParams :: MForm site (Maybe Env)
askParams = view $ to (fmap fst . mfdParams)

askFiles :: MForm site (Maybe FileEnv)
askFiles = view $ to (fmap snd . mfdParams)

-- | Converts a form field into monadic form 'WForm'. This field requires a
-- value and will return 'FormFailure' if left empty.
--
-- @since 1.4.14
wreq :: RenderMessage site FormMessage
     => Field site a        -- ^ form field
     -> FieldSettings site  -- ^ settings for this field
     -> Maybe a             -- ^ optional default value
     -> WForm site (FormResult a)
wreq f fs = mFormToWForm . mreq f fs

-- | Converts a form field into monadic form 'WForm'. This field is optional,
-- i.e.  if filled in, it returns 'Just a', if left empty, it returns
-- 'Nothing'.  Arguments are the same as for 'wreq' (apart from type of default
-- value).
--
-- @since 1.4.14
wopt :: Field site a        -- ^ form field
     -> FieldSettings site  -- ^ settings for this field
     -> Maybe (Maybe a)     -- ^ optional default value
     -> WForm site (FormResult (Maybe a))
wopt f fs = mFormToWForm . mopt f fs

-- | Converts a monadic form 'WForm' into an applicative form 'AForm'.
--
-- @since 1.4.14
wFormToAForm
  :: WForm site (FormResult a)  -- ^ input form
  -> AForm site a               -- ^ output form
wFormToAForm = formToAForm . wFormToMForm

-- | Converts a monadic form 'WForm' into another monadic form 'MForm'.
--
-- @since 1.4.14
wFormToMForm
  :: WForm site a                      -- ^ input form
  -> MForm site (a, [FieldView site])  -- ^ output form
wFormToMForm wform = do
  viewsDeque <- newDeque
  mfd <- view id
  a <- runRIO (WFormData viewsDeque mfd) wform
  views <- dequeToList viewsDeque
  pure (a, views)

-- | Converts a monadic form 'MForm' into another monadic form 'WForm'.
--
-- @since 1.4.14
mFormToWForm
  :: MForm site (a, FieldView site)  -- ^ input form
  -> WForm site a                    -- ^ output form
mFormToWForm mform = do
  WFormData viewsDeque mfd <- view id
  (a, view') <- runRIO mfd mform
  pushBackDeque viewsDeque view'
  pure a

-- | Converts a form field into monadic form. This field requires a value
-- and will return 'FormFailure' if left empty.
mreq :: RenderMessage site FormMessage
     => Field site a         -- ^ form field
     -> FieldSettings site  -- ^ settings for this field
     -> Maybe a             -- ^ optional default value
     -> MForm site (FormResult a, FieldView site)
mreq field fs mdef = mhelper field fs mdef (\m l -> FormFailure [renderMessage m l MsgValueRequired]) FormSuccess True

-- | Converts a form field into monadic form. This field is optional, i.e.
-- if filled in, it returns 'Just a', if left empty, it returns 'Nothing'.
-- Arguments are the same as for 'mreq' (apart from type of default value).
mopt :: Field site a
     -> FieldSettings site
     -> Maybe (Maybe a)
     -> MForm site (FormResult (Maybe a), FieldView site)
mopt field fs mdef = mhelper field fs (join mdef) (const $ const $ FormSuccess Nothing) (FormSuccess . Just) False

mhelper :: Field site a
        -> FieldSettings site
        -> Maybe a
        -> (site -> [Text] -> FormResult b) -- ^ on missing
        -> (a -> FormResult b) -- ^ on success
        -> Bool -- ^ is it required?
        -> MForm site (FormResult b, FieldView site)

mhelper Field {..} FieldSettings {..} mdef onMissing onFound isReq = do
    tell fieldEnctype
    mp <- askParams
    name <- maybe newFormIdent return fsName
    theId <- maybe newIdent return fsId
    site <- getYesod
    langs <- reqLangs <$> getRequest
    let mr2 = renderMessage site langs
    (res, val) <-
        case mp of
            Nothing -> return (FormMissing, maybe (Left "") Right mdef)
            Just p -> do
                mfs <- askFiles
                let mvals = fromMaybe [] $ Map.lookup name p
                    files = fromMaybe [] $ mfs >>= Map.lookup name
                emx <- liftHandler $ fieldParse mvals files
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
areq :: RenderMessage site FormMessage
     => Field site a
     -> FieldSettings site
     -> Maybe a
     -> AForm site a
areq a b = formToAForm . liftM (second return) . mreq a b

-- | Applicative equivalent of 'mopt'.
aopt :: Field site a
     -> FieldSettings site
     -> Maybe (Maybe a)
     -> AForm site (Maybe a)
aopt a b = formToAForm . liftM (second return) . mopt a b

runFormGeneric
  :: HasHandlerData env
  => MForm (HandlerSite env) a
  -> Maybe (Env, FileEnv)
  -> RIO env (a, Enctype)
runFormGeneric mform params = do
  hd <- liftHandler $ view subHandlerDataL
  enctypeRef <- newIORef mempty
  intsRef <- newIORef $! IntSingle 0
  let mfd = MFormData
        { mfdHandlerData = hd
        , mfdEnctype = enctypeRef
        , mfdParams = params
        , mfdInts = intsRef
        }
  a <- runRIO mfd mform
  (,) a <$> readIORef enctypeRef

-- | This function is used to both initially render a form and to later extract
-- results from it. Note that, due to CSRF protection and a few other issues,
-- forms submitted via GET and POST are slightly different. As such, be sure to
-- call the relevant function based on how the form will be submitted, /not/
-- the current request method.
--
-- For example, a common case is displaying a form on a GET request and having
-- the form submit to a POST page. In such a case, both the GET and POST
-- handlers should use 'runFormPost'.
runFormPost
  :: (RenderMessage (HandlerSite env) FormMessage, HasHandlerData env)
  => (Html -> MForm (HandlerSite env) (FormResult a, xml))
  -> RIO env ((FormResult a, xml), Enctype)
runFormPost form = do
    env <- postEnv
    postHelper form env

postHelper
  :: (HasHandlerData env, RenderMessage (HandlerSite env) FormMessage)
  => (Html -> MForm (HandlerSite env) (FormResult a, xml))
  -> Maybe (Env, FileEnv)
  -> RIO env ((FormResult a, xml), Enctype)
postHelper form env = do
    req <- getRequest
    let tokenKey = defaultCsrfParamName
    let token =
            case reqToken req of
                Nothing -> Data.Monoid.mempty
                Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]
    ((res, xml), enctype) <- runFormGeneric (form token) env
    site <- getYesod
    let res' =
            case (res, env) of
                (_, Nothing) -> FormMissing
                (FormSuccess{}, Just (params, _))
                    | not (Map.lookup tokenKey params === reqToken req) ->
                        FormFailure [renderMessage site (reqLangs req) MsgCsrfWarning]
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
    :: (RenderMessage (HandlerSite env) FormMessage, HasHandlerData env)
    => (Html -> MForm (HandlerSite env) (FormResult a, xml))
    -> RIO env (xml, Enctype)
generateFormPost form = first snd `liftM` postHelper form Nothing

postEnv :: HasHandlerData env => RIO env (Maybe (Env, FileEnv))
postEnv = do
    req <- getRequest
    if requestMethod (reqWaiRequest req) == "GET"
        then return Nothing
        else do
            (p, f) <- runRequestBody
            let p' = Map.unionsWith (++) $ map (\(x, y) -> Map.singleton x [y]) p
            return $ Just (p', Map.unionsWith (++) $ map (\(k, v) -> Map.singleton k [v]) f)

runFormPostNoToken :: HasHandlerData env
                   => (Html -> MForm (HandlerSite env) a)
                   -> RIO env (a, Enctype)
runFormPostNoToken form = do
    params <- postEnv
    runFormGeneric (form mempty) params

runFormGet :: HasHandlerData env
           => (Html -> MForm (HandlerSite env) a)
           -> RIO env (a, Enctype)
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
    :: HasHandlerData env
    => (Html -> MForm (HandlerSite env) (FormResult a, xml))
    -> RIO env (xml, Enctype)
generateFormGet' form = first snd `liftM` getHelper form Nothing

{-# DEPRECATED generateFormGet "Will require RenderMessage in next version of Yesod" #-}
generateFormGet :: HasHandlerData env
                => (Html -> MForm (HandlerSite env) a)
                -> RIO env (a, Enctype)
generateFormGet form = getHelper form Nothing

getKey :: Text
getKey = "_hasdata"

getHelper :: HasHandlerData env
          => (Html -> MForm (HandlerSite env) a)
          -> Maybe (Env, FileEnv)
          -> RIO env (a, Enctype)
getHelper form params = do
    let fragment = [shamlet|<input type=hidden name=#{getKey}>|]
    runFormGeneric (form fragment) params


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
  :: Text -- ^ Form identification string.
  -> (Html -> MForm site (FormResult a, WidgetFor site ()))
  -> (Html -> MForm site (FormResult a, WidgetFor site ()))
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
    let eraseParams | missing   = local (const Nothing)
                    | otherwise = id
    ( res', w) <- eraseParams (form fragment')

    -- Empty forms now properly return FormMissing. [#1072](https://github.com/yesodweb/yesod/issues/1072)
    let res = if missing then FormMissing else res'
    return ( res, w)

identifyFormKey :: Text
identifyFormKey = "_formid"


type FormRender site a =
       AForm site a
    -> Html
    -> MForm site (FormResult a, WidgetFor site ())

renderTable, renderDivs, renderDivsNoLabels :: FormRender env a
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

renderDivsMaybeLabels :: Bool -> FormRender env a
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
renderBootstrap2 :: FormRender env a
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

check :: RenderMessage site msg
      => (a -> Either msg a)
      -> Field site a
      -> Field site a
check f = checkM $ return . f

-- | Return the given error message if the predicate is false.
checkBool :: RenderMessage site msg
          => (a -> Bool) -> msg -> Field site a -> Field site a
checkBool b s = check $ \x -> if b x then Right x else Left s

checkM :: RenderMessage site msg
       => (a -> HandlerFor site (Either msg a))
       -> Field site a
       -> Field site a
checkM f = checkMMap f id

-- | Same as 'checkM', but modifies the datatype.
--
-- In order to make this work, you must provide a function to convert back from
-- the new datatype to the old one (the second argument to this function).
--
-- Since 1.1.2
checkMMap :: RenderMessage site msg
          => (a -> HandlerFor site (Either msg b))
          -> (b -> a)
          -> Field site a
          -> Field site b
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
customErrorMessage :: SomeMessage site -> Field site a -> Field site a
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
convertField :: (a -> b) -> (b -> a)
             -> Field env a -> Field env b
convertField to' from (Field fParse fView fEnctype) = let
  fParse' ts = fmap (fmap (fmap to')) . fParse ts
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
