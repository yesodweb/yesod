{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-- | A module providing a means of creating multiple input forms, such as a
-- list of 0 or more recipients.
module Yesod.Form.MassInput
    ( inputList
    , massDivs
    , massTable
    ) where

import Yesod.Form.Types
import Yesod.Form.Functions
import Yesod.Form.Fields (checkBoxField)
import Yesod.Core
import Control.Monad.Trans.RWS (get, put, ask)
import Data.Maybe (fromMaybe)
import Data.Text.Read (decimal)
import Control.Monad (liftM)
import Data.Either (partitionEithers)
import Data.Traversable (sequenceA)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)

down :: Monad m => Int -> MForm m ()
down 0 = return ()
down i | i < 0 = error "called down with a negative number"
down i = do
    is <- get
    put $ IntCons 0 is
    down $ i - 1

up :: Monad m => Int -> MForm m ()
up 0 = return ()
up i | i < 0 = error "called down with a negative number"
up i = do
    is <- get
    case is of
        IntSingle _ -> error "up on IntSingle"
        IntCons _ is' -> put is' >> newFormIdent >> return ()
    up $ i - 1

-- | Generate a form that accepts 0 or more values from the user, allowing the
-- user to specify that a new row is necessary.
inputList :: (xml ~ WidgetFor site (), RenderMessage site FormMessage)
          => Html
          -- ^ label for the form
          -> ([[FieldView site]] -> xml)
          -- ^ how to display the rows, usually either 'massDivs' or 'massTable'
          -> (Maybe a -> AForm (HandlerFor site) a)
          -- ^ display a single row of the form, where @Maybe a@ gives the
          -- previously submitted value
          -> Maybe [a]
          -- ^ default initial values for the form
          -> AForm (HandlerFor site) [a]
inputList label fixXml single mdef = formToAForm $ do
    theId <- lift newIdent
    down 1
    countName <- newFormIdent
    addName <- newFormIdent
    (menv, _, _) <- ask
    let readInt t =
            case decimal t of
                Right (i, "") -> Just i
                _ -> Nothing
    let vals =
            case menv of
                Nothing -> map Just $ fromMaybe [] mdef
                Just (env, _) ->
                    let toAdd = maybe False (const True) $ Map.lookup addName env
                        count' = fromMaybe 0 $ Map.lookup countName env >>= listToMaybe >>= readInt
                        count = (if toAdd then 1 else 0) + count'
                     in replicate count Nothing
    let count = length vals
    (res, xmls, views) <- liftM fixme $ mapM (withDelete . single) vals
    up 1
    return (res, [FieldView
        { fvLabel = label
        , fvTooltip = Nothing
        , fvId = theId
        , fvInput = [whamlet|
$newline never
^{fixXml views}
<p>
    $forall xml <- xmls
        ^{xml}
    <input .count type=hidden name=#{countName} value=#{count}>
    <input type=checkbox name=#{addName}>
    Add another row
|]
        , fvErrors = Nothing
        , fvRequired = False
        }])

withDelete :: (xml ~ WidgetFor site (), RenderMessage site FormMessage)
           => AForm (HandlerFor site) a
           -> MForm (HandlerFor site) (Either xml (FormResult a, [FieldView site]))
withDelete af = do
    down 1
    deleteName <- newFormIdent
    (menv, _, _) <- ask
    res <- case menv >>= Map.lookup deleteName . fst of
        Just ("yes":_) -> return $ Left [whamlet|
$newline never
<input type=hidden name=#{deleteName} value=yes>
|]
        _ -> do
            (_, xml2) <- aFormToForm $ areq checkBoxField FieldSettings
                { fsLabel = SomeMessage MsgDelete
                , fsTooltip = Nothing
                , fsName = Just deleteName
                , fsId = Nothing
                , fsAttrs = []
                } $ Just False
            (res, xml) <- aFormToForm af
            return $ Right (res, xml $ xml2 [])
    up 1
    return res

fixme :: [Either xml (FormResult a, [FieldView site])]
      -> (FormResult [a], [xml], [[FieldView site]])
fixme eithers =
    (res, xmls, map snd rest)
  where
    (xmls, rest) = partitionEithers eithers
    res = Data.Traversable.sequenceA $ map fst rest

massDivs, massTable
         :: [[FieldView site]]
         -> WidgetFor site ()
massDivs viewss = [whamlet|
$newline never
$forall views <- viewss
    <fieldset>
        $forall view <- views
            <div :fvRequired view:.required :not $ fvRequired view:.optional>
                <label for=#{fvId view}>#{fvLabel view}
                $maybe tt <- fvTooltip view
                    <div .tooltip>#{tt}
                ^{fvInput view}
                $maybe err <- fvErrors view
                    <div .errors>#{err}
|]

massTable viewss = [whamlet|
$newline never
$forall views <- viewss
    <fieldset>
        <table>
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
