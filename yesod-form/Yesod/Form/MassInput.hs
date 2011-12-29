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
import Yesod.Form.Fields (boolField)
import Yesod.Widget (GWidget, whamlet)
import Yesod.Message (RenderMessage)
import Yesod.Handler (newIdent, GHandler)
import Text.Blaze (Html)
import Control.Monad.Trans.Class (lift)
import Data.Text (pack)
import Control.Monad.Trans.RWS (get, put, ask)
import Data.Maybe (fromMaybe)
import Data.Text.Read (decimal)
import Control.Monad (liftM)
import Data.Either (partitionEithers)
import Data.Traversable (sequenceA)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)

#if __GLASGOW_HASKELL__ >= 700
#define WHAMLET whamlet
#define HTML html
#else
#define HTML $html
#define WHAMLET $whamlet
#endif

down :: Int -> MForm sub master ()
down 0 = return ()
down i | i < 0 = error "called down with a negative number"
down i = do
    is <- get
    put $ IntCons 0 is
    down $ i - 1

up :: Int -> MForm sub master ()
up 0 = return ()
up i | i < 0 = error "called down with a negative number"
up i = do
    is <- get
    case is of
        IntSingle _ -> error "up on IntSingle"
        IntCons _ is' -> put is' >> newFormIdent >> return ()
    up $ i - 1

inputList :: (m ~ GHandler sub master, xml ~ GWidget sub master (), RenderMessage master FormMessage)
          => Html
          -> ([[FieldView sub master]] -> xml)
          -> (Maybe a -> AForm sub master a)
          -> (Maybe [a] -> AForm sub master [a])
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

withDelete :: (xml ~ GWidget sub master (), RenderMessage master FormMessage)
           => AForm sub master a
           -> MForm sub master (Either xml (FormResult a, [FieldView sub master]))
withDelete af = do
    down 1
    deleteName <- newFormIdent
    (menv, _, _) <- ask
    res <- case menv >>= Map.lookup deleteName . fst of
        Just ("yes":_) -> return $ Left [WHAMLET|<input type=hidden name=#{deleteName} value=yes>|]
        _ -> do
            (_, xml2) <- aFormToForm $ areq boolField FieldSettings
                { fsLabel = MsgDelete
                , fsTooltip = Nothing
                , fsName = Just deleteName
                , fsId = Nothing
                , fsClass = []
                } $ Just False
            (res, xml) <- aFormToForm af
            return $ Right (res, xml $ xml2 [])
    up 1
    return res

fixme :: (xml ~ GWidget sub master ())
      => [Either xml (FormResult a, [FieldView sub master])]
      -> (FormResult [a], [xml], [[FieldView sub master]])
fixme eithers =
    (res, xmls, map snd rest)
  where
    (xmls, rest) = partitionEithers eithers
    res = sequenceA $ map fst rest

massDivs, massTable
         :: [[FieldView sub master]]
         -> GWidget sub master ()
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
