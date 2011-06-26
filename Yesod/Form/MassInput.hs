{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Form.MassInput
    ( inputList
    , massDivs
    , massTable
    ) where

import Yesod.Form.Types
import Yesod.Form.Functions
import Yesod.Form.Fields (boolField, FormMessage)
import Yesod.Widget (GGWidget, whamlet)
import Yesod.Message (RenderMessage)
import Yesod.Handler (newIdent, GGHandler)
import Text.Blaze (Html)
import Control.Monad.Trans.Class (lift)
import Data.Text (pack, Text)
import Control.Monad.Trans.RWS (get, put, ask)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text.Read (decimal)
import Control.Monad (liftM)
import Data.Either (partitionEithers)
import Data.Traversable (sequenceA)

#if __GLASGOW_HASKELL__ >= 700
#define WHAMLET whamlet
#define HTML html
#else
#define HTML $html
#define WHAMLET $whamlet
#endif

down 0 = return ()
down i | i < 0 = error "called down with a negative number"
down i = do
    is <- get
    put $ IntCons 0 is
    down $ i - 1

up 0 = return ()
up i | i < 0 = error "called down with a negative number"
up i = do
    is <- get
    case is of
        IntSingle _ -> error "up on IntSingle"
        IntCons _ is' -> put is' >> newFormIdent >> return ()
    up $ i - 1

inputList :: (Monad mo, m ~ GGHandler sub master mo, xml ~ GGWidget master (GGHandler sub master mo) (), RenderMessage master FormMessage)
          => Html
          -> ([[FieldView xml]] -> xml)
          -> (Maybe a -> AForm ([FieldView xml] -> [FieldView xml]) master m a)
          -> (Maybe [a] -> AForm ([FieldView xml] -> [FieldView xml]) master m [a])
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
                    let toAdd = maybe False (const True) $ lookup addName env
                        count' = fromMaybe 0 $ lookup countName env >>= readInt
                        count = (if toAdd then 1 else 0) + count'
                     in replicate count Nothing
    let count = length vals
    (res, xmls, views) <- liftM fixme $ mapM (withDelete . single) vals
    up 1
    return (res, FieldView
        { fvLabel = label
        , fvTooltip = Nothing
        , fvId = pack theId
        , fvInput = [WHAMLET|
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
        })

withDelete :: (xml ~ GGWidget master m (), m ~ GGHandler sub master mo, Monad mo, RenderMessage master FormMessage)
           => AForm ([FieldView xml] -> [FieldView xml]) master m a
           -> Form master m (Either xml (FormResult a, [FieldView xml]))
withDelete af = do
    down 1
    deleteName <- newFormIdent
    (menv, _, _) <- ask
    res <- case menv >>= lookup deleteName . fst of
        Just "yes" -> return $ Left [WHAMLET|<input type=hidden name=#{deleteName} value=yes>|]
        _ -> do
            (_, xml2) <- aFormToForm $ areq boolField FieldSettings
                { fsLabel = "Delete?" :: Text -- FIXME
                , fsTooltip = Nothing
                , fsName = Just deleteName
                , fsId = Nothing
                } $ Just False -- TRANS
            (res, xml) <- aFormToForm af
            return $ Right (res, xml [] ++ xml2 []) -- FIXME shouldn't need ++
    up 1
    return res

fixme :: (xml ~ GGWidget master (GGHandler sub master mo) ())
      => [Either xml (FormResult a, [FieldView xml])]
      -> (FormResult [a], [xml], [[FieldView xml]])
fixme eithers =
    (res, xmls, map snd rest)
  where
    (xmls, rest) = partitionEithers eithers
    res = sequenceA $ map fst rest

massDivs, massTable
         :: Monad m
         => [[FieldView (GGWidget master m ())]]
         -> GGWidget master m ()
massDivs viewss = [WHAMLET|
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

massTable viewss = [WHAMLET|
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
