{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE FlexibleContexts #-}

module Yesod.Form.NewMassInput where

import Yesod.Form.Types
import Yesod.Form.Functions
import Yesod.Form.Fields (intField)
import Yesod.Core
import Control.Monad.Trans.RWS (ask, tell)
import qualified Data.Map as Map
import Control.Monad (liftM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust, listToMaybe, fromMaybe)
import Control.Arrow (second)

-- | Applicative equivalent of 'mmulti'.
amulti :: (site ~ HandlerSite m, MonadHandler m, RenderMessage site FormMessage)
    => Field m a
    -> FieldSettings site
    -> [a]
    -> Int
    -> AForm m [a]
amulti field fs defs minVals = formToAForm $ liftM (second return) (mmulti field fs defs minVals)

-- | Converts a form field into a monadic form containing an arbitrary
-- number of the given fields as specified by the user. Returns a list
-- of results, failing if the length of the list is less than the minimum
-- requested values.
mmulti :: (site ~ HandlerSite m, MonadHandler m, RenderMessage site FormMessage)
    => Field m a
    -> FieldSettings site
    -> [a]
    -> Int
    -> MForm m (FormResult [a], FieldView site)
mmulti field fs defs minVals = mhelperMulti field fs defs minVals

-- Helper function, performs a bounds check on minVals and adds a class to
-- the FieldSettings for identification later.
mhelperMulti :: (site ~ HandlerSite m, MonadHandler m, RenderMessage site FormMessage)
    => Field m a
    -> FieldSettings site
    -> [a]
    -> Int
    -> MForm m (FormResult [a], FieldView site)
mhelperMulti field fs@FieldSettings {..} defs minVals = do
    fieldClass <- newFormIdent
    let fs' = fs {fsAttrs = addClass fieldClass fsAttrs}
        minVals' = if minVals < 0 then 0 else minVals
    mhelperMulti' field fs' fieldClass defs minVals'

-- Helper function, does most of the work for mmulti.
mhelperMulti' :: (site ~ HandlerSite m, MonadHandler m, RenderMessage site FormMessage)
    => Field m a
    -> FieldSettings site
    -> Text
    -> [a]
    -> Int
    -> MForm m (FormResult [a], FieldView site)
mhelperMulti' field@Field {..} fs@FieldSettings {..} fieldClass defs minVals = do
    mp <- askParams
    (_, site, langs) <- ask
    name <- maybe newFormIdent return fsName
    theId <- maybe newFormIdent return fsId
    cName <- newFormIdent
    cid <- newFormIdent
    btnId <- newFormIdent

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
    
    -- get counter value
    cr@(cRes, _) <- case mp of
        Nothing -> return (FormMissing, Right cDef)
        Just p -> mkRes intField cfs p mfs cName onMissingFail FormSuccess

    -- generate counter view
    cView <- mkView intField cfs cr cid cName True

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

    -- generate field views
    (rs, fvs) <- do
        let mkView' ((n,i), r@(res, _)) = do
                fv <- mkView field fs r i n False
                return (res, fv)
            xs = zip (mkNames counter) results
            notSuccNothing (_, (r,_)) = not $ isSuccNothing r
            ys = case filter notSuccNothing xs of
                [] -> [((mkName 0, mkId 0), (FormSuccess Nothing, Left ""))] -- always need at least one value to generate a field
                zs -> zs
        rvs <- mapM mkView' ys
        return $ unzip rvs
    
    -- generate form widget
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

        widget = do
            [whamlet|
                ^{fvInput cView}

                $forall fv <- fvs
                    ^{fvInput fv}
                    $maybe err <- fvErrors fv
                        <span .help-block .error-block>#{err}

                <button ##{btnId} type="button">Add Another
            |]
            toWidget
                [julius|
                    var extraFields = 0;
                    $("#" + #{btnId}).click(function() {
                        extraFields++;
                        var newNumber = parseInt(#{show counter}) + extraFields;
                        $("#" + #{cid}).val(newNumber);
                        var newName = #{name} + "-" + newNumber;
                        var newId = #{theId} + "-" + newNumber;
                        
                        var newElem = $("." + #{fieldClass}).first().clone();
                        newElem.val("").attr('name', newName).attr('id', newId);
                        newElem.insertBefore("#" + #{btnId})
                    });
                |]

    let view = FieldView
            { fvLabel = toHtml $ mr2 fsLabel
            , fvTooltip = fmap toHtml $ fmap mr2 fsTooltip
            , fvId = theId
            , fvInput = widget
            , fvErrors = if tooFewVals then Just $ toHtml err else Nothing
            , fvRequired = False
            }

    return (res, view)

-- Search for the given field's name in the environment
-- and parse any values found and construct a FormResult.
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
    -> Text
    -> Text
    -> Bool
    -> MForm m (FieldView site)
mkView Field {..} FieldSettings {..} (res, val) theId name isReq = do
    (_, site, langs) <- ask
    let mr2 = renderMessage site langs
    return $ FieldView
        { fvLabel = toHtml $ mr2 fsLabel
        , fvTooltip = fmap toHtml $ fmap mr2 fsTooltip
        , fvId = theId
        , fvInput = fieldView theId name fsAttrs val isReq
        , fvErrors =
            case res of
                FormFailure [e] -> Just $ toHtml e
                _ -> Nothing
        , fvRequired = isReq
        }