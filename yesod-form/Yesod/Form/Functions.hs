{-# LANGUAGE QuasiQuotes #-}
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
      -- * Fields to Forms
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
    , generateFormGet
      -- * Rendering
    , FormRender
    , renderTable
    , renderDivs
    , renderDivsNoLabels 
    , renderBootstrap
      -- * Validation
    , check
    , checkBool
    , checkM
    , checkMMap
    , checkMMod
    , customErrorMessage
      -- * Utilities
    , fieldSettingsLabel
    , aformM
    , parseHelper
    ) where

import Control.Monad.Trans.Resource (MonadResource)
import Yesod.Form.Types
import Data.Text (Text, pack)
import Control.Arrow (second)
import Control.Monad.Trans.RWS (ask, get, put, runRWST, tell, evalRWST)
import Control.Monad (liftM, join)
import Crypto.Classes (constTimeEq)
import Text.Blaze (Markup, toMarkup)
#define Html Markup
#define toHtml toMarkup
import Yesod.Core
import Network.Wai (requestMethod)
import Text.Hamlet (shamlet)
import Data.Monoid (mempty)
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE
import Control.Arrow (first)

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
formToAForm form = AForm $ \(site, langs) env ints -> do
    ((a, xmls), ints', enc) <- runRWST form (env, site, langs) ints
    return (a, (++) xmls, ints', enc)

aFormToForm :: AForm site a -> MForm site (FormResult a, [FieldView site] -> [FieldView site])
aFormToForm (AForm aform) = do
    ints <- get
    (env, site, langs) <- ask
    (a, xml, ints', enc) <- lift $ aform (site, langs) env ints
    put ints'
    tell enc
    return (a, xml)

askParams :: MForm site (Maybe Env)
askParams = do
    (x, _, _) <- ask
    return $ liftM fst x

askFiles :: MForm site (Maybe FileEnv)
askFiles = do
    (x, _, _) <- ask
    return $ liftM snd x

mreq :: RenderMessage site FormMessage
     => Field site a -> FieldSettings site -> Maybe a
     -> MForm site (FormResult a, FieldView site)
mreq field fs mdef = mhelper field fs mdef (\m l -> FormFailure [renderMessage m l MsgValueRequired]) FormSuccess True

mopt :: Field site a -> FieldSettings site -> Maybe (Maybe a)
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

areq :: RenderMessage site FormMessage
     => Field site a -> FieldSettings site -> Maybe a
     -> AForm site a
areq a b = formToAForm . fmap (second return) . mreq a b

aopt :: Field site a
     -> FieldSettings site
     -> Maybe (Maybe a)
     -> AForm site (Maybe a)
aopt a b = formToAForm . fmap (second return) . mopt a b

runFormGeneric :: MonadHandler m
               => MForm (HandlerSite m) a
               -> HandlerSite m
               -> [Text]
               -> Maybe (Env, FileEnv)
               -> m (a, Enctype)
runFormGeneric form site langs env = liftHandler $ evalRWST form (env, site, langs) (IntSingle 1)

-- | This function is used to both initially render a form and to later extract
-- results from it. Note that, due to CSRF protection and a few other issues,
-- forms submitted via GET and POST are slightly different. As such, be sure to
-- call the relevant function based on how the form will be submitted, /not/
-- the current request method.
--
-- For example, a common case is displaying a form on a GET request and having
-- the form submit to a POST page. In such a case, both the GET and POST
-- handlers should use 'runFormPost'.
runFormPost :: (HandlerSite m ~ site, RenderMessage site FormMessage, MonadResource m, MonadHandler m)
            => (Html -> MForm site (FormResult a, xml))
            -> m ((FormResult a, xml), Enctype)
runFormPost form = do
    env <- postEnv
    postHelper form env

postHelper  :: (site ~ HandlerSite m, RenderMessage site FormMessage, MonadHandler m)
            => (Html -> MForm site (FormResult a, xml))
            -> Maybe (Env, FileEnv)
            -> m ((FormResult a, xml), Enctype)
postHelper form env = do
    req <- getRequest
    let tokenKey = "_token"
    let token =
            case reqToken req of
                Nothing -> mempty
                Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]
    m <- getYesod
    langs <- languages
    ((res, xml), enctype) <- runFormGeneric (form token) m langs env
    let res' =
            case (res, env) of
                (FormSuccess{}, Just (params, _))
                    | not (Map.lookup tokenKey params === reqToken req) ->
                        FormFailure [renderMessage m langs MsgCsrfWarning]
                _ -> res
            where (Just [t1]) === (Just t2) = TE.encodeUtf8 t1 `constTimeEq` TE.encodeUtf8 t2
                  Nothing     === Nothing   = True   -- It's important to use constTimeEq
                  _           === _         = False  -- in order to avoid timing attacks.
    return ((res', xml), enctype)

-- | Similar to 'runFormPost', except it always ignore the currently available
-- environment. This is necessary in cases like a wizard UI, where a single
-- page will both receive and incoming form and produce a new, blank form. For
-- general usage, you can stick with @runFormPost@.
generateFormPost
    :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage)
    => (Html -> MForm (HandlerSite m) (FormResult a, xml))
    -> m (xml, Enctype)
generateFormPost form = first snd `liftM` postHelper form Nothing

postEnv :: (HandlerState m, MonadResource m)
        => m (Maybe (Env, FileEnv))
postEnv = do
    req <- getRequest
    if requestMethod (reqWaiRequest req) == "GET"
        then return Nothing
        else do
            (p, f) <- runRequestBody
            let p' = Map.unionsWith (++) $ map (\(x, y) -> Map.singleton x [y]) p
            return $ Just (p', Map.unionsWith (++) $ map (\(k, v) -> Map.singleton k [v]) f)

runFormPostNoToken :: (MonadHandler m)
                   => (Html -> MForm (HandlerSite m) (FormResult a, xml))
                   -> m ((FormResult a, xml), Enctype)
runFormPostNoToken form = do
    langs <- languages
    m <- getYesod
    env <- postEnv
    runFormGeneric (form mempty) m langs env

runFormGet :: MonadHandler m
           => (Html -> MForm (HandlerSite m) a)
           -> m (a, Enctype)
runFormGet form = do
    gets <- liftM reqGetParams getRequest
    let env =
            case lookup getKey gets of
                Nothing -> Nothing
                Just _ -> Just (Map.unionsWith (++) $ map (\(x, y) -> Map.singleton x [y]) gets, Map.empty)
    getHelper form env

generateFormGet :: MonadHandler m
                => (Html -> MForm (HandlerSite m) a)
                -> m (a, Enctype)
generateFormGet form = getHelper form Nothing

getKey :: Text
getKey = "_hasdata"

getHelper :: MonadHandler m
          => (Html -> MForm (HandlerSite m) a)
          -> Maybe (Env, FileEnv)
          -> m (a, Enctype)
getHelper form env = do
    let fragment = [shamlet|<input type=hidden name=#{getKey}>|]
    langs <- languages
    m <- getYesod
    runFormGeneric (form fragment) m langs env

type FormRender site a =
       AForm site a
    -> Html
    -> MForm site (FormResult a, GWidget site ())

renderTable, renderDivs, renderDivsNoLabels :: FormRender site a
renderTable aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
    let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <tr :fvRequired view:.required :not $ fvRequired view:.optional>
        <td>
            <label for=#{fvId view}>#{fvLabel view}
            $maybe tt <- fvTooltip view
                <div .tooltip>#{tt}
        <td>^{fvInput view}
        $maybe err <- fvErrors view
            <td .errors>#{err}
|]
    return (res, widget)

-- | render a field inside a div
renderDivs = renderDivsMaybeLabels True

-- | render a field inside a div, not displaying any label
renderDivsNoLabels = renderDivsMaybeLabels False

renderDivsMaybeLabels :: Bool -> FormRender site a
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

-- | Render a form using Bootstrap-friendly shamlet syntax.
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
renderBootstrap :: FormRender site a
renderBootstrap aform fragment = do
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
      => (a -> Either msg a) -> Field site a -> Field site a
check f = checkM $ return . f

-- | Return the given error message if the predicate is false.
checkBool :: RenderMessage site msg
          => (a -> Bool) -> msg -> Field site a -> Field site a
checkBool b s = check $ \x -> if b x then Right x else Left s

checkM :: RenderMessage site msg
       => (a -> GHandler site (Either msg a))
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
          => (a -> GHandler site (Either msg b))
          -> (b -> a)
          -> Field site a
          -> Field site b
checkMMap f inv field = field
    { fieldParse = \ts fs -> do
        e1 <- fieldParse field ts fs
        case e1 of
            Left msg -> return $ Left msg
            Right Nothing -> return $ Right Nothing
            Right (Just a) -> fmap (either (Left . SomeMessage) (Right . Just)) $ f a
    , fieldView = \i n a eres req -> fieldView field i n a (fmap inv eres) req
    }

-- | Deprecated synonym for 'checkMMap'.
--
-- Since 1.1.1
checkMMod :: RenderMessage site msg
          => (a -> GHandler site (Either msg b))
          -> (b -> a)
          -> Field site a
          -> Field site b
checkMMod = checkMMap
{-# DEPRECATED checkMMod "Please use checkMMap instead" #-}

-- | Allows you to overwrite the error message on parse error.
customErrorMessage :: SomeMessage site -> Field site a -> Field site a
customErrorMessage msg field = field { fieldParse = \ts fs -> fmap (either
(const $ Left msg) Right) $ fieldParse field ts fs }

-- | Generate a 'FieldSettings' from the given label.
fieldSettingsLabel :: RenderMessage site msg => msg -> FieldSettings site
fieldSettingsLabel msg = FieldSettings (SomeMessage msg) Nothing Nothing Nothing []

-- | Generate an 'AForm' that gets its value from the given action.
aformM :: GHandler site a -> AForm site a
aformM action = AForm $ \_ _ ints -> do
    value <- action
    return (FormSuccess value, id, ints, mempty)

-- | A helper function for creating custom fields.
--
-- This is intended to help with the common case where a single input value is
-- required, such as when parsing a text field.
--
-- Since 1.1
parseHelper :: (Monad m, RenderMessage site FormMessage)
            => (Text -> Either FormMessage a)
            -> [Text] -> [FileInfo] -> m (Either (SomeMessage site) (Maybe a))
parseHelper _ [] _ = return $ Right Nothing
parseHelper _ ("":_) _ = return $ Right Nothing
parseHelper f (x:_) _ = return $ either (Left . SomeMessage) (Right . Just) $ f x
