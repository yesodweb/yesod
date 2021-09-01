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
import Text.Julius (rawJS)
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

-- | By default delete buttons have a @margin-left@ property of @0.75rem@.
-- You can override this by specifying an alternative value in a class
-- which is then passed inside 'MultiSettings'.
--
-- @since 1.7.0
data MultiSettings site = MultiSettings
    { msAddClass :: !Text -- ^ Class to be applied to the "add another" button.
    , msDelClass :: !Text -- ^ Class to be applied to the "delete" button.
    , msTooltipClass :: Text -- ^ Only used in applicative forms. Class to be applied to the tooltip.
    , msWrapperErrClass :: !Text -- ^ Class to be applied to the wrapper if it's field has an error.
    , msAddInner :: !(Maybe Html) -- ^ Inner Html of add button, defaults to "Add Another". Useful for adding icons inside buttons.
    , msDelInner :: !(Maybe Html) -- ^ Inner Html of delete button, defaults to "Delete". Useful for adding icons inside buttons.
    , msErrWidget :: Maybe (Html -> WidgetFor site ()) -- ^ Only used in applicative forms. Create a widget for displaying errors.
    }

-- | The general structure of each individually generated field is as follows.
-- There is an external wrapper element containing both an inner wrapper and any
-- error messages that apply to that specific field. The inner wrapper contains
-- both the field and it's corresponding delete button.
--
-- The structure is illustrated by the following:
-- 
-- > <div .#{wrapperClass}>
-- >     <div .#{wrapperClass}-inner>
-- >         ^{fieldWidget}
-- >         ^{deleteButton}
-- >     ^{maybeErrorMessages}
--
-- Each wrapper element has the same class which is automatically generated. This class
-- is returned in the 'MultiView' should you wish to change the styling. The inner wrapper
-- uses the same class followed by @-inner@. By default the wrapper and inner wrapper has
-- classes are as follows:
-- 
-- > .#{wrapperClass} {
-- >     margin-bottom: 1rem;
-- > }
-- >
-- > .#{wrapperClass}-inner {
-- >     display: flex;
-- >     flex-direction: row;
-- > }
--
-- @since 1.7.0
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
bs3Settings = MultiSettings
    "btn btn-default"
    "btn btn-danger"
    "help-block"
    "has-error"
    Nothing Nothing (Just errW)
    where
        errW err = 
            [whamlet|
                <span .help-block>#{err}
            |]

-- | 'MultiSettings' for Bootstrap 4.
--
-- @since 1.6.0
bs4Settings :: MultiSettings site
bs4Settings = MultiSettings
    "btn btn-secondary"
    "btn btn-danger"
    "form-text text-muted"
    "has-error"
    Nothing Nothing (Just errW)
    where
        errW err =
            [whamlet|
                <div .invalid-feedback>#{err}
            |]

-- | 'MultiSettings' for Bootstrap 3 with Font Awesome 5 Icons.
-- Uses @fa-plus@ for the add button and @fa-trash-alt@ for the delete button.
--
-- @since 1.7.0
bs3FASettings :: MultiSettings site
bs3FASettings = MultiSettings
    "btn btn-default"
    "btn btn-danger"
    "help-block"
    "has-error"
    addIcon delIcon (Just errW)
    where
        addIcon = Just [shamlet|<i class="fas fa-plus">|]
        delIcon = Just [shamlet|<i class="fas fa-trash-alt">|]
        errW err = 
            [whamlet|
                <span .help-block>#{err}
            |]

-- | 'MultiSettings' for Bootstrap 4 with Font Awesome 5 Icons.
-- Uses @fa-plus@ for the add button and @fa-trash-alt@ for the delete button.
--
-- @since 1.7.0
bs4FASettings :: MultiSettings site
bs4FASettings = MultiSettings
    "btn btn-secondary"
    "btn btn-danger"
    "form-text text-muted"
    "has-error"
    addIcon delIcon (Just errW)
    where
        addIcon = Just [shamlet|<i class="fas fa-plus">|]
        delIcon = Just [shamlet|<i class="fas fa-trash-alt">|]
        errW err =
            [whamlet|
                <div .invalid-feedback>#{err}
            |]

-- | Applicative equivalent of 'mmulti'.
--
-- Note about tooltips:
-- Rather than displaying the tooltip alongside each field the
-- tooltip is displayed once at the top of the multi-field set.
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

            let (fv : _) = mvFields
                widget = do
                    [whamlet|
                        $maybe tooltip <- fvTooltip fv
                            <small .#{msTooltipClass ms}>#{tooltip}

                        ^{fvInput mvCounter}

                        $forall fv <- mvFields
                            ^{fvInput fv}

                        ^{fvInput mvAddBtn}
                    |]
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
    wrapperClass <- lift newIdent
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
    theId <- lift $ maybe newIdent return fsId
    cName <- newFormIdent
    cid <- lift newIdent
    addBtnId <- lift newIdent
    delBtnPrefix <- lift newIdent

    let mr2 = renderMessage site langs
        cDef = length defs
        cfs = FieldSettings "" Nothing (Just cid) (Just cName) [("hidden", "true")]
        mkName i = name `T.append` (T.pack $ '-' : show i)
        mkId i = theId `T.append` (T.pack $ '-' : show i)
        mkNames c = [(i, (mkName i, mkId i)) | i <- [0 .. c]]
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
        Just p -> mapM
            (\n -> mkRes field fs p mfs n onMissingSucc (FormSuccess . Just))
            (map (fst . snd) $ mkNames counter)

    -- delete button

    -- The delFunction is included down with the add button rather than with
    -- each delete button to ensure that the function only gets included once.
    let delFunction = toWidget
            [julius|
                function deleteField_#{rawJS theId}(wrapper) {
                    var numFields = $('.#{rawJS wrapperClass}').length;

                    if (numFields == 1)
                    {
                        wrapper.find("*").each(function() {
                            removeVals($(this));
                        });
                    }
                    else
                        wrapper.remove();
                }

                function removeVals(e) {
                    // input types where we don't want to reset the value
                    const keepValueTypes = ["radio", "checkbox", "button"];

                    var shouldKeep = keepValueTypes.includes(e.prop('type'))
                                        || e.prop("tagName") == "OPTION";

                    // uncheck any checkboxes or radio fields and empty any text boxes
                    if(e.prop('checked') == true)
                        e.prop('checked', false);

                    if(!shouldKeep)
                        e.val("").trigger("change");
                        // trigger change is to ensure WYSIWYG editors are updated
                        // when their hidden code field is cleared
                }
            |]

        mkDelBtn fieldId = do
            let delBtnId = delBtnPrefix `T.append` fieldId
            [whamlet|
                <button ##{delBtnId} .#{msDelClass} style="margin-left: 0.75rem" type="button">
                    $maybe inner <- msDelInner
                        #{inner}
                    $nothing
                        Delete
            |]
            toWidget
                [julius|
                    $('##{rawJS delBtnId}').click(function() {
                        var field = $('##{rawJS fieldId}');
                        deleteField_#{rawJS theId}(field.parents('.#{rawJS wrapperClass}'));
                    });                    
                |]

    -- generate field views
    (rs, fvs) <- do
        let mkView' ((c, (n,i)), r@(res, _)) = do
                let del = Just (mkDelBtn i, wrapperClass, c)
                fv <- mkView field fs r del msErrWidget msWrapperErrClass i n True
                return (res, fv)
            xs = zip (mkNames counter) results
            notSuccNothing (_, (r,_)) = not $ isSuccNothing r
            ys = case filter notSuccNothing xs of
                [] -> [((0, (mkName 0, mkId 0)), (FormSuccess Nothing, Left ""))] -- always need at least one value to generate a field
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
                    var extraFields_#{rawJS theId} = 0;
                    $('##{rawJS addBtnId}').click(function() {
                        extraFields_#{rawJS theId}++;
                        var newNumber = parseInt(#{show counter}) + extraFields_#{rawJS theId};
                        $("#" + #{cid}).val(newNumber);
                        var newName = #{name} + "-" + newNumber;
                        var newId = #{theId} + "-" + newNumber;
                        var newDelId = #{delBtnPrefix} + newId;
                        
                        // get new wrapper and remove old error messages
                        var newWrapper = $('.#{rawJS wrapperClass}').first().clone();
                        newWrapper.children( ':not(.#{rawJS wrapperClass}-inner)' ).remove();
                        newWrapper.removeClass(#{msWrapperErrClass});

                        // get counter from wrapper
                        var oldCount = newWrapper.data("counter");
                        var oldName = #{name} + "-" + oldCount;
                        var oldId = #{theId} + "-" + oldCount;
                        var oldDelBtn = #{delBtnPrefix} + oldId;

                        // replace any id, name or for attributes that began with
                        // the old values and replace them with the new values
                        var idRegex = new RegExp("^" + oldId);
                        var nameRegex = new RegExp("^" + oldName);

                        var els = newWrapper.find("*");
                        els.each(function() {
                            var e = $(this);

                            if(e.prop('id') != undefined)
                                e.prop('id', e.prop('id').replace(idRegex, newId));

                            if(e.prop('name') != undefined)
                                e.prop('name', e.prop('name').replace(nameRegex, newName));

                            if(e.prop('for') != undefined)
                                e.prop('for', e.prop('for').replace(idRegex, newId)); // radio fields use id in for attribute

                            removeVals(e);
                        });

                        // set new counter on wrapper
                        newWrapper.attr("data-counter", newNumber);

                        var newDelBtn = newWrapper.find('[id^=#{rawJS delBtnPrefix}]');
                        newDelBtn.prop('id', newDelId);
                        newDelBtn.click(() => deleteField_#{rawJS theId}(newWrapper));

                        newWrapper.insertBefore('##{rawJS addBtnId}');
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
    -- Delete button widget, class for div wrapping each field with it's delete button and counter value for that field.
    -- Nothing if the field passed doesn't need a delete button e.g. if it is the counter field.
    -> Maybe (WidgetFor site (), Text, Int)
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
                $maybe (delBtn, wrapperClass, counter) <- mdel
                    <div .#{wrapperClass} :isJust merr:.#{errClass} data-counter=#{counter}>
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