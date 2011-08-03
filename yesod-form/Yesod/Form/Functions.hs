{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Form.Functions
    ( -- * Running in Form monad
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
    , runFormPostNoNonce
    , runFormGet
      -- * Rendering
    , FormRender
    , renderTable
    , renderDivs
      -- * Validation
    , check
    , checkBool
    , checkM
    ) where

import Yesod.Form.Types
import Yesod.Form.Fields (FormMessage (MsgCsrfWarning, MsgValueRequired))
import Data.Text (Text, pack)
import Control.Monad.Trans.RWS (ask, get, put, runRWST, tell, evalRWST)
import Control.Monad.Trans.Class (lift)
import Control.Monad (liftM, join)
import Text.Blaze (Html, toHtml)
import Yesod.Handler (GHandler, GGHandler, getRequest, runRequestBody, newIdent, getYesod)
import Yesod.Core (RenderMessage, liftIOHandler)
import Yesod.Widget (GWidget, GGWidget, whamlet)
import Yesod.Request (reqNonce, reqWaiRequest, reqGetParams, languages)
import Network.Wai (requestMethod)
import Text.Hamlet (html)
import Data.Monoid (mempty)
import Data.Maybe (listToMaybe)
import Yesod.Message (RenderMessage (..))
import Control.Monad.IO.Class (MonadIO, liftIO)

#if __GLASGOW_HASKELL__ >= 700
#define WHAMLET whamlet
#define HTML html
#else
#define HTML $html
#define WHAMLET $whamlet
#endif

-- | Get a unique identifier.
newFormIdent :: Form sub master Text
newFormIdent = do
    i <- get
    let i' = incrInts i
    put i'
    return $ pack $ 'f' : show i'
  where
    incrInts (IntSingle i) = IntSingle $ i + 1
    incrInts (IntCons i is) = (i + 1) `IntCons` is

formToAForm :: Form sub master (FormResult a, FieldView sub master) -> AForm sub master a
formToAForm form = AForm $ \(master, langs) env ints -> do
    ((a, xml), ints', enc) <- runRWST form (env, master, langs) ints
    return (a, (:) xml, ints', enc)

aFormToForm :: AForm sub master a -> Form sub master (FormResult a, [FieldView sub master] -> [FieldView sub master])
aFormToForm (AForm aform) = do
    ints <- get
    (env, master, langs) <- ask
    (a, xml, ints', enc) <- lift $ aform (master, langs) env ints
    put ints'
    tell enc
    return (a, xml)

askParams :: Form sub master (Maybe Env)
askParams = do
    (x, _, _) <- ask
    return $ liftM fst x

askFiles :: Form sub master (Maybe FileEnv)
askFiles = do
    (x, _, _) <- ask
    return $ liftM snd x

mreq :: (RenderMessage master msg, RenderMessage master FormMessage)
     => Field sub master a -> FieldSettings msg -> Maybe a
     -> Form sub master (FormResult a, FieldView sub master)
mreq field fs mdef = mhelper field fs mdef (\m l -> FormFailure [renderMessage m l MsgValueRequired]) FormSuccess True

mopt :: RenderMessage master msg
     => Field sub master a -> FieldSettings msg -> Maybe (Maybe a)
     -> Form sub master (FormResult (Maybe a), FieldView sub master)
mopt field fs mdef = mhelper field fs (join mdef) (const $ const $ FormSuccess Nothing) (FormSuccess . Just) False

mhelper :: RenderMessage master msg
        => Field sub master a
        -> FieldSettings msg
        -> Maybe a
        -> (master -> [Text] -> FormResult b) -- ^ on missing
        -> (a -> FormResult b) -- ^ on success
        -> Bool -- ^ is it required?
        -> Form sub master (FormResult b, FieldView sub master)

mhelper Field {..} FieldSettings {..} mdef onMissing onFound isReq = do
    mp <- askParams
    name <- maybe newFormIdent return fsName
    theId <- lift $ maybe (liftM pack newIdent) return fsId
    (_, master, langs) <- ask
    let mr2 = renderMessage master langs
    (res, val) <-
        case mp of
            Nothing -> return (FormMissing, maybe (Left "") Right mdef)
            Just p -> do
                let mvals = map snd $ filter (\(n,_) -> n == name) p
                emx <- lift $ fieldParse mvals
                return $ case emx of
                    Left (SomeMessage e) -> (FormFailure [renderMessage master langs e], maybe (Left "") Left (listToMaybe mvals))
                    Right mx ->
                        case mx of
                            Nothing -> (onMissing master langs, Left "")
                            Just x -> (onFound x, Right x)
    return (res, FieldView
        { fvLabel = toHtml $ mr2 fsLabel
        , fvTooltip = fmap toHtml $ fmap mr2 fsTooltip
        , fvId = theId
        , fvInput = fieldView theId name val isReq
        , fvErrors =
            case res of
                FormFailure [e] -> Just $ toHtml e
                _ -> Nothing
        , fvRequired = isReq
        })

areq :: (RenderMessage master msg, RenderMessage master FormMessage)
     => Field sub master a -> FieldSettings msg -> Maybe a
     -> AForm sub master a
areq a b = formToAForm . mreq a b

aopt :: RenderMessage master msg
     => Field sub master a
     -> FieldSettings msg
     -> Maybe (Maybe a)
     -> AForm sub master (Maybe a)
aopt a b = formToAForm . mopt a b

runFormGeneric :: MonadIO m => Form sub master a -> master -> [Text] -> Maybe (Env, FileEnv) -> GGHandler sub master m (a, Enctype)
runFormGeneric form master langs env = liftIOHandler $ evalRWST form (env, master, langs) (IntSingle 1)

-- | This function is used to both initially render a form and to later extract
-- results from it. Note that, due to CSRF protection and a few other issues,
-- forms submitted via GET and POST are slightly different. As such, be sure to
-- call the relevant function based on how the form will be submitted, /not/
-- the current request method.
--
-- For example, a common case is displaying a form on a GET request and having
-- the form submit to a POST page. In such a case, both the GET and POST
-- handlers should use 'runFormPost'.
runFormPost :: RenderMessage master FormMessage
            => (Html -> Form sub master (FormResult a, xml))
            -> GHandler sub master ((FormResult a, xml), Enctype)
runFormPost form = do
    req <- getRequest
    let nonceKey = "_nonce"
    let nonce =
            case reqNonce req of
                Nothing -> mempty
                Just n -> [HTML|<input type=hidden name=#{nonceKey} value=#{n}>|]
    env <- if requestMethod (reqWaiRequest req) == "GET"
                then return Nothing
                else fmap Just runRequestBody
    m <- getYesod
    langs <- languages
    ((res, xml), enctype) <- runFormGeneric (form nonce) m langs env
    let res' =
            case (res, env) of
                (FormSuccess{}, Just (params, _))
                    | lookup nonceKey params /= reqNonce req ->
                        FormFailure [renderMessage m langs MsgCsrfWarning]
                _ -> res
    return ((res', xml), enctype)

runFormPostNoNonce :: (Html -> Form sub master (FormResult a, xml)) -> GHandler sub master ((FormResult a, xml), Enctype)
runFormPostNoNonce form = do
    req <- getRequest
    env <- if requestMethod (reqWaiRequest req) == "GET"
                then return Nothing
                else fmap Just runRequestBody
    langs <- languages
    m <- getYesod
    runFormGeneric (form mempty) m langs env

runFormGet :: (Html -> Form sub master a) -> GHandler sub master (a, Enctype)
runFormGet form = do
    let key = "_hasdata"
    let fragment = [HTML|<input type=hidden name=#{key}>|]
    gets <- liftM reqGetParams getRequest
    let env =
            case lookup key gets of
                Nothing -> Nothing
                Just _ -> Just (gets, [])
    langs <- languages
    m <- getYesod
    runFormGeneric (form fragment) m langs env

type FormRender sub master a =
       AForm sub master a
    -> Html
    -> Form sub master (FormResult a, GWidget sub master ())

renderTable, renderDivs :: FormRender sub master a
renderTable aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
    -- FIXME non-valid HTML
    let widget = [WHAMLET|
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

renderDivs aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
    let widget = [WHAMLET|
\#{fragment}
$forall view <- views
    <div :fvRequired view:.required :not $ fvRequired view:.optional>
        <label for=#{fvId view}>#{fvLabel view}
        $maybe tt <- fvTooltip view
            <div .tooltip>#{tt}
        ^{fvInput view}
        $maybe err <- fvErrors view
            <div .errors>#{err}
|]
    return (res, widget)

check :: RenderMessage master msg
      => (a -> Either msg a) -> Field sub master a -> Field sub master a
check f = checkM $ return . f

-- | Return the given error message if the predicate is false.
checkBool :: RenderMessage master msg
          => (a -> Bool) -> msg -> Field sub master a -> Field sub master a
checkBool b s = check $ \x -> if b x then Right x else Left s

checkM :: RenderMessage master msg
       => (a -> GGHandler sub master IO (Either msg a))
       -> Field sub master a
       -> Field sub master a
checkM f field = field
    { fieldParse = \ts -> do
        e1 <- fieldParse field ts
        case e1 of
            Left msg -> return $ Left msg
            Right Nothing -> return $ Right Nothing
            Right (Just a) -> fmap (either (Left . SomeMessage) (Right . Just)) $ f a
    }
