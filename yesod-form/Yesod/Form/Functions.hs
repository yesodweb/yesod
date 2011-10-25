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
      -- * Generate a blank form
    , generateFormPost
    , generateFormGet
      -- * Rendering
    , FormRender
    , renderTable
    , renderDivs
      -- * Validation
    , check
    , checkBool
    , checkM
    , customErrorMessage
    ) where

import Yesod.Form.Types
import Data.Text (Text, pack)
import Control.Monad.Trans.RWS (ask, get, put, runRWST, tell, evalRWST)
import Control.Monad.Trans.Class (lift)
import Control.Monad (liftM, join)
import Text.Blaze (Html, toHtml)
import Yesod.Handler (GHandler, GGHandler, getRequest, runRequestBody, newIdent, getYesod)
import Yesod.Core (RenderMessage, liftIOHandler, SomeMessage (..))
import Yesod.Widget (GWidget, whamlet)
import Yesod.Request (reqNonce, reqWaiRequest, reqGetParams, languages, FileInfo (..))
import Network.Wai (requestMethod)
import Text.Hamlet (shamlet)
import Data.Monoid (mempty)
import Data.Maybe (listToMaybe, fromMaybe)
import Yesod.Message (RenderMessage (..))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as L

#if __GLASGOW_HASKELL__ >= 700
#define WHAMLET whamlet
#define HTML shamlet
#else
#define HTML $shamlet
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
                let mvals = fromMaybe [] $ Map.lookup name p
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
        , fvInput = fieldView theId name fsClass val isReq
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
    env <- postEnv
    postHelper form env

postHelper  :: RenderMessage master FormMessage
            => (Html -> Form sub master (FormResult a, xml))
            -> Maybe (Env, FileEnv)
            -> GHandler sub master ((FormResult a, xml), Enctype)
postHelper form env = do
    req <- getRequest
    let nonceKey = "_nonce"
    let nonce =
            case reqNonce req of
                Nothing -> mempty
                Just n -> [HTML|<input type=hidden name=#{nonceKey} value=#{n}>|]
    m <- getYesod
    langs <- languages
    ((res, xml), enctype) <- runFormGeneric (form nonce) m langs env
    let res' =
            case (res, env) of
                (FormSuccess{}, Just (params, _))
                    | Map.lookup nonceKey params /= fmap return (reqNonce req) ->
                        FormFailure [renderMessage m langs MsgCsrfWarning]
                _ -> res
    return ((res', xml), enctype)

-- | Similar to 'runFormPost', except it always ignore the currently available
-- environment. This is necessary in cases like a wizard UI, where a single
-- page will both receive and incoming form and produce a new, blank form. For
-- general usage, you can stick with @runFormPost@.
generateFormPost
    :: RenderMessage master FormMessage
    => (Html -> Form sub master (FormResult a, xml))
    -> GHandler sub master ((FormResult a, xml), Enctype)
generateFormPost form = postHelper form Nothing

postEnv :: GHandler sub master (Maybe (Env, FileEnv))
postEnv = do
    req <- getRequest
    if requestMethod (reqWaiRequest req) == "GET"
        then return Nothing
        else do
            (p, f) <- runRequestBody
            let p' = Map.unionsWith (++) $ map (\(x, y) -> Map.singleton x [y]) p
            return $ Just (p', Map.fromList $ filter (notEmpty . snd) f)
  where
    notEmpty = not . L.null . fileContent

runFormPostNoNonce :: (Html -> Form sub master (FormResult a, xml)) -> GHandler sub master ((FormResult a, xml), Enctype)
runFormPostNoNonce form = do
    langs <- languages
    m <- getYesod
    env <- postEnv
    runFormGeneric (form mempty) m langs env

runFormGet :: (Html -> Form sub master a) -> GHandler sub master (a, Enctype)
runFormGet form = do
    gets <- liftM reqGetParams getRequest
    let env =
            case lookup getKey gets of
                Nothing -> Nothing
                Just _ -> Just (Map.unionsWith (++) $ map (\(x, y) -> Map.singleton x [y]) gets, Map.empty)
    getHelper form env

generateFormGet :: (Html -> Form sub master a) -> GHandler sub master (a, Enctype)
generateFormGet form = getHelper form Nothing

getKey :: Text
getKey = "_hasdata"

getHelper :: (Html -> Form sub master a) -> Maybe (Env, FileEnv) -> GHandler sub master (a, Enctype)
getHelper form env = do
    let fragment = [HTML|<input type=hidden name=#{getKey}>|]
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

-- | Allows you to overwrite the error message on parse error.
customErrorMessage :: SomeMessage master -> Field sub master a -> Field sub master a
customErrorMessage msg field = field { fieldParse = \ts -> fmap (either
(const $ Left msg) Right) $ fieldParse field ts }
