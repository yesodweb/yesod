{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

-- | A module providing a means of creating multiple input forms without
-- the need to submit the form to generate a new input field unlike
-- in "MassInput".
module Yesod.Form.MultiInput
    ( MultiSettings (..)
    , MultiView (..)
    , mmulti
    , amulti
    , bs3Settings
    , bs3FASettings
    , bs4Settings
    , bs4FASettings
    ) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Control.Monad.Trans.RWS (ask, tell)
import qualified Data.Map as Map
import Data.Maybe (fromJust, listToMaybe, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Yesod.Core
import Yesod.Form.Fields (intField)
import Yesod.Form.Functions
import Yesod.Form.Types

#ifdef MIN_VERSION_shakespeare(2,0,18)
#if MIN_VERSION_shakespeare(2,0,18)
#else
import Text.Julius (ToJavascript (..))
instance ToJavascript String where toJavascript = toJavascript . toJSON
instance ToJavascript Text where toJavascript = toJavascript . toJSON
#endif
#endif

-- @since 1.6.0
data MultiSettings site = MultiSettings
    { msAddClass :: Text -- ^ Class to be applied to the "add another" button.
    , msDelClass :: Text -- ^ Class to be applied to the "delete" button.
    , msWrapperErrClass :: Text -- ^ Class to be applied to the wrapper if it's field has an error.
    , msAddInner :: Maybe Html -- ^ Inner Html of add button, defaults to "Add Another". Useful for adding icons inside buttons.
    , msDelInner :: Maybe Html -- ^ Inner Html of delete button, defaults to "Delete". Useful for adding icons inside buttons.
    , msErrWidget :: Maybe (Html -> WidgetFor site ()) -- ^ Only used in applicative forms. Create a widget for displaying errors.
    }

-- @since 1.6.0
data MultiView site = MultiView
    { mvCounter :: FieldView site -- ^ Hidden counter field.
    , mvFields :: [FieldView site] -- ^ Input fields.
    , mvAddBtn :: FieldView site -- ^ Button to add another field.
    , mvWrapperClass :: Text -- ^ Class applied to a div wrapping each field with it's delete button.
    }

-- | 'MultiSettings' for Bootstrap 3.
--
-- @since 1.6.0
bs3Settings :: MultiSettings site
bs3Settings = MultiSettings "btn btn-default" "btn btn-danger" "has-error" Nothing Nothing (Just errW)
    where
        errW err = 
            [whamlet|
                <span .help-block>#{err}
            |]

-- | 'MultiSettings' for Bootstrap 4.
--
-- @since 1.6.0
bs4Settings :: MultiSettings site
bs4Settings = MultiSettings "btn btn-basic" "btn btn-danger" "has-error" Nothing Nothing (Just errW)
    where
        errW err =
            [whamlet|
                <div .invalid-feedback>#{err}
            |]

-- | 'MultiSettings' for Bootstrap 3 with Font Awesome 5 Icons.
--
-- @since 1.6.0
bs3FASettings :: MultiSettings site
bs3FASettings = MultiSettings "btn btn-default" "btn btn-danger" "has-error" addIcon delIcon (Just errW)
    where
        addIcon = Just [shamlet|<i class="fas fa-plus">|]
        delIcon = Just [shamlet|<i class="fas fa-trash-alt">|]
        errW err = 
            [whamlet|
                <span .help-block>#{err}
            |]

-- | 'MultiSettings' for Bootstrap 4 with Font Awesome 5 Icons.
--
-- @since 1.6.0
bs4FASettings :: MultiSettings site
bs4FASettings = MultiSettings "btn btn-basic" "btn btn-danger" "has-error" addIcon delIcon (Just errW)
    where
        addIcon = Just [shamlet|<i class="fas fa-plus">|]
        delIcon = Just [shamlet|<i class="fas fa-trash-alt">|]
        errW err =
            [whamlet|
                <div .invalid-feedback>#{err}
            |]

-- | Applicative equivalent of 'mmulti'.
--
-- @since 1.6.0
amulti :: (site ~ HandlerSite m, MonadHandler m, RenderMessage site FormMessage)
    => Field m a
    -> FieldSettings site
    -> [a]
    -> Int
    -> MultiSettings site
    -> AForm m [a]
amulti field fs defs minVals ms = formToAForm $
    liftM (second return) mform
    where
        mform = do
            (fr, MultiView {..}) <- mmulti field fs defs minVals ms

            let widget = do
                    [whamlet|
                        ^{fvInput mvCounter}

                        $forall fv <- mvFields
                            ^{fvInput fv}

                        ^{fvInput mvAddBtn}
                    |]
                (fv : _) = mvFields
                view = FieldView
                    { fvLabel = fvLabel fv
                    , fvTooltip = Nothing
                    , fvId = fvId fv
                    , fvInput = widget
                    , fvErrors = fvErrors mvAddBtn
                    , fvRequired = False
                    }
            
            return (fr, view)

-- | Converts a form field into a monadic form containing an arbitrary
-- number of the given fields as specified by the user. Returns a list
-- of results, failing if the length of the list is less than the minimum
-- requested values.
--
-- @since 1.6.0
mmulti :: (site ~ HandlerSite m, MonadHandler m, RenderMessage site FormMessage)
    => Field m a
    -> FieldSettings site
    -> [a]
    -> Int
    -> MultiSettings site
    -> MForm m (FormResult [a], MultiView site)
mmulti field fs defs minVals' ms = do
    wrapperClass <- newFormIdent
    let minVals = if minVals' < 0 then 0 else minVals'
    mhelperMulti field fs wrapperClass defs minVals ms

-- Helper function, does most of the work for mmulti.
mhelperMulti :: (site ~ HandlerSite m, MonadHandler m, RenderMessage site FormMessage)
    => Field m a
    -> FieldSettings site
    -> Text
    -> [a]
    -> Int
    -> MultiSettings site
    -> MForm m (FormResult [a], MultiView site)
mhelperMulti field@Field {..} fs@FieldSettings {..} wrapperClass defs minVals MultiSettings {..} = do
    mp <- askParams
    (_, site, langs) <- ask
    name <- maybe newFormIdent return fsName
    theId <- maybe newFormIdent return fsId
    cName <- newFormIdent
    cid <- newFormIdent
    addBtnId <- newFormIdent
    delBtnPrefix <- newFormIdent

    let mr2 = renderMessage site langs
        cDef = length defs
        cfs = FieldSettings "" Nothing (Just cid) (Just cName) [("hidden", "true")]
        mkName i = name `T.append` (T.pack $ '-' : show i)
        mkId i = theId `T.append` (T.pack $ '-' : show i)
        mkNames c = [(mkName i, mkId i) | i <- [0 .. c]]
        onMissingSucc _ _ = FormSuccess Nothing
        onMissingFail m l = FormFailure [renderMessage m l MsgValueRequired]
        isSuccNothing r = case r of
            FormSuccess Nothing -> True
            _ -> False

    mfs <- askFiles
    
    -- get counter value (starts counting from 0)
    cr@(cRes, _) <- case mp of
        Nothing -> return (FormMissing, Right cDef)
        Just p -> mkRes intField cfs p mfs cName onMissingFail FormSuccess

    -- generate counter view
    cView <- mkView intField cfs cr Nothing Nothing msWrapperErrClass cid cName True

    let counter = case cRes of
            FormSuccess c -> c
            _             -> cDef

    -- get results of fields
    results <- case mp of
        Nothing -> return $
            if cDef == 0
                then [(FormMissing, Left "")]
                else [(FormMissing, Right d) | d <- defs]
        Just p -> mapM (\n -> mkRes field fs p mfs n onMissingSucc (FormSuccess . Just)) (map fst $ mkNames counter)

    -- delete button

    -- The delFunction is included down with the add button rather than with
    -- each delete button to ensure that the function only gets included once.
    let delFunction = toWidget
            [julius|
                function deleteField(field) {
                    var numFields = $("." + #{wrapperClass}).length;

                    if (numFields == 1)
                        field.val("");
                    else
                        field.parent().parent().remove();
                }
            |]

        mkDelBtn fieldId = do
            let delBtnId = delBtnPrefix <> fieldId
            [whamlet|
                <button ##{delBtnId} .#{msDelClass} style="margin-left: 1rem" type="button">
                    $maybe inner <- msDelInner
                        #{inner}
                    $nothing
                        Delete
            |]
            toWidget
                [julius|
                    $("#" + #{delBtnId}).click(function() {
                        var field = $("#" + #{fieldId});
                        deleteField(field);
                    });                    
                |]

    -- generate field views
    (rs, fvs) <- do
        let mkView' ((n,i), r@(res, _)) = do
                let del = Just (mkDelBtn i, wrapperClass)
                fv <- mkView field fs r del msErrWidget msWrapperErrClass i n False
                return (res, fv)
            xs = zip (mkNames counter) results
            notSuccNothing (_, (r,_)) = not $ isSuccNothing r
            ys = case filter notSuccNothing xs of
                [] -> [((mkName 0, mkId 0), (FormSuccess Nothing, Left ""))] -- always need at least one value to generate a field
                zs -> zs
        rvs <- mapM mkView' ys
        return $ unzip rvs
    
    -- check values
    let rs' = [ fmap fromJust r | r <- rs
                                , not $ isSuccNothing r ]
        err = T.pack $ "Please enter at least " ++ show minVals ++ " values."
        (res, tooFewVals) = 
            case foldr (<*>) (FormSuccess []) (map (fmap $ (:)) rs') of
                FormSuccess xs ->
                    if length xs < minVals
                        then (FormFailure [err], True)
                        else (FormSuccess xs, False)
                fRes -> (fRes, False)
    
        -- create add button
        -- also includes some styling / functions that we only want to include once
        btnWidget = do
            [whamlet|
                <button ##{addBtnId} .#{msAddClass} type="button">
                    $maybe inner <- msAddInner
                        #{inner}
                    $nothing
                        Add Another
            |]
            toWidget
                [lucius|
                    .#{wrapperClass} {
                        margin-bottom: 1rem;
                    }
                    .#{wrapperClass}-inner {
                        display: flex;
                        flex-direction: row;
                    }
                |]
            delFunction -- function used by delete buttons, included here so that it only gets included once
            toWidget
                [julius|
                    var extraFields = 0;
                    $("#" + #{addBtnId}).click(function() {
                        extraFields++;
                        var newNumber = parseInt(#{show counter}) + extraFields;
                        $("#" + #{cid}).val(newNumber);
                        var newName = #{name} + "-" + newNumber;
                        var newId = #{theId} + "-" + newNumber;
                        var newDelId = #{delBtnPrefix} + newId;
                        
                        var newWrapper = $("." + #{wrapperClass}).first().clone();
                        newWrapper.children( ":not(." + #{wrapperClass} + "-inner)" ).remove(); // remove error messages

                        var newField = newWrapper.find("[id^=" + #{theId} + "]"); 
                        newField.val("").attr('name', newName).attr('id', newId);

                        var newDelBtn = newWrapper.find("[id^=" + #{delBtnPrefix} + "]");
                        newDelBtn.attr('id', newDelId);
                        newDelBtn.click(() => deleteField(newField));

                        newWrapper.insertBefore("#" + #{addBtnId});
                    });
                |]

        btnView = FieldView
            { fvLabel = toHtml $ mr2 ("" :: Text)
            , fvTooltip = Nothing
            , fvId = addBtnId
            , fvInput = btnWidget
            , fvErrors = if tooFewVals then Just $ toHtml err else Nothing
            , fvRequired = False
            }

    return (res, MultiView cView fvs btnView wrapperClass)

-- Search for the given field's name in the environment,
-- parse any values found and construct a FormResult.
mkRes :: (site ~ HandlerSite m, MonadHandler m)
    => Field m a
    -> FieldSettings site
    -> Env
    -> Maybe FileEnv
    -> Text
    -> (site -> [Text] -> FormResult b)
    -> (a -> FormResult b)
    -> MForm m (FormResult b, Either Text a)
mkRes Field {..} FieldSettings {..} p mfs name onMissing onFound = do
    tell fieldEnctype
    (_, site, langs) <- ask
    let mvals = fromMaybe [] $ Map.lookup name p
        files = fromMaybe [] $ mfs >>= Map.lookup name
    emx <- lift $ fieldParse mvals files
    return $ case emx of
        Left msg -> (FormFailure [renderMessage site langs msg], maybe (Left "") Left (listToMaybe mvals))
        Right mx ->
            case mx of
                Nothing -> (onMissing site langs, Left "") 
                Just x -> (onFound x, Right x)

-- Generate a FieldView for the given field with the given result.
mkView :: (site ~ HandlerSite m, MonadHandler m)
    => Field m a
    -> FieldSettings site
    -> (FormResult b, Either Text a)
    -> Maybe (WidgetFor site (), Text) -- Delete button widget and class for div wrapping each field with it's delete button.
    -> Maybe (Html -> WidgetFor site ()) -- Function to display error messages.
    -> Text
    -> Text
    -> Text
    -> Bool
    -> MForm m (FieldView site)
mkView Field {..} FieldSettings {..} (res, val) mdel merrW errClass theId name isReq = do
    (_, site, langs) <- ask
    let mr2 = renderMessage site langs
        merr = case res of
                FormFailure [e] -> Just $ toHtml e
                _ -> Nothing
        fv' = fieldView theId name fsAttrs val isReq
        fv = do
            [whamlet|
                $maybe (delBtn, wrapperClass) <- mdel
                    <div .#{wrapperClass} :isJust merr:.#{errClass}>
                        <div .#{wrapperClass}-inner>
                            ^{fv'}
                            ^{delBtn}
                            
                        $maybe err <- merr
                            $maybe errW <- merrW
                                ^{errW err}
                        
                $nothing
                    ^{fv'}
            |]
    return $ FieldView
        { fvLabel = toHtml $ mr2 fsLabel
        , fvTooltip = fmap toHtml $ fmap mr2 fsTooltip
        , fvId = theId
        , fvInput = fv
        , fvErrors = merr
        , fvRequired = isReq
        }