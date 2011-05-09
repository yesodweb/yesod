{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
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
    ) where

import Yesod.Form.Types
import Data.Text (Text, pack)
import Control.Monad.Trans.RWS (ask, get, put, runRWST, tell, evalRWST)
import Control.Monad.Trans.Class (lift)
import Control.Monad (liftM)
import Text.Blaze (Html)
import Yesod.Handler (GHandler, GGHandler, getRequest, runRequestBody)
import Yesod.Widget (GGWidget, whamlet)
import Yesod.Request (reqNonce, reqWaiRequest, reqGetParams)
import Network.Wai (requestMethod)
import Text.Hamlet.NonPoly (html)
import Data.Monoid (mempty)

#if __GLASGOW_HASKELL__ >= 700
#define WHAMLET whamlet
#define HTML html
#else
#define HTML $html
#define WHAMLET $whamlet
#endif

-- | Get a unique identifier.
newFormIdent :: Monad m => Form m Text
newFormIdent = do
    i <- get
    let i' = incrInts i
    put i'
    return $ pack $ 'f' : show i'
  where
    incrInts (IntSingle i) = IntSingle $ i + 1
    incrInts (IntCons i is) = (i + 1) `IntCons` is

formToAForm :: Monad m => Form m (FormResult a, xml) -> AForm ([xml] -> [xml]) m a
formToAForm form = AForm $ \env ints -> do
    ((a, xml), ints', enc) <- runRWST form env ints
    return (a, (:) xml, ints', enc)

aFormToForm :: Monad m => AForm xml m a -> Form m (FormResult a, xml)
aFormToForm (AForm aform) = do
    ints <- get
    env <- ask
    (a, xml, ints', enc) <- lift $ aform env ints
    put ints'
    tell enc
    return (a, xml)

askParams :: Monad m => Form m (Maybe Env)
askParams = liftM (liftM fst) ask

askFiles :: Monad m => Form m (Maybe FileEnv)
askFiles = liftM (liftM snd) ask

mreq :: Monad m => Field xml a -> Maybe a -> Form m (FormResult a, xml)
mreq = undefined

mopt :: Monad m => Field xml a -> Maybe (Maybe a) -> Form m (FormResult (Maybe a), xml)
mopt = undefined

areq :: Monad m => Field xml a -> Maybe a -> AForm ([xml] -> [xml]) m a
areq a b = formToAForm $ mreq a b

aopt :: Monad m => Field xml a -> Maybe (Maybe a) -> AForm ([xml] -> [xml]) m (Maybe a)
aopt a b = formToAForm $ mopt a b

runFormGeneric :: Monad m => Form m a -> Maybe (Env, FileEnv) -> m (a, Enctype)
runFormGeneric form env = evalRWST form env (IntSingle 1)

runFormPost :: (Html -> Form (GHandler sub master) (FormResult a, xml)) -> GHandler sub master ((FormResult a, xml), Enctype)
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
    ((res, xml), enctype) <- runFormGeneric (form nonce) env
    let res' =
            case (res, env) of
                (FormSuccess{}, Just (params, _))
                    | lookup nonceKey params /= reqNonce req ->
                        FormFailure [csrfWarning]
                _ -> res
    return ((res', xml), enctype)

csrfWarning :: Text
csrfWarning = "As a protection against cross-site request forgery attacks, please confirm your form submission." -- TRANS

runFormPostNoNonce :: (Html -> Form (GHandler sub master) a) -> GHandler sub master (a, Enctype)
runFormPostNoNonce form = do
    req <- getRequest
    env <- if requestMethod (reqWaiRequest req) == "GET"
                then return Nothing
                else fmap Just runRequestBody
    runFormGeneric (form mempty) env

runFormGet :: Monad m => (Html -> Form (GGHandler sub master m) a) -> GGHandler sub master m (a, Enctype)
runFormGet form = do
    let key = "_hasdata"
    let fragment = [HTML|<input type=hidden name=#{key}>|]
    gets <- liftM reqGetParams getRequest
    let env =
            case lookup key gets of
                Nothing -> Nothing
                Just _ -> Just (gets, [])
    runFormGeneric (form fragment) env

type FormRender master m a =
       AForm ([FieldView (GGWidget master m ())] -> [FieldView (GGWidget master m ())]) m a
    -> Html
    -> Form m (FormResult a, GGWidget master m ())

renderTable, renderDivs :: Monad m => FormRender master m a
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
