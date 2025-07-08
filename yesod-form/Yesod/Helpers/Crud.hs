{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Helpers.Crud
    ( Item (..)
    , Crud (..)
    , CrudRoute (..)
    , defaultCrud
    ) where

import Yesod.Core
import Text.Hamlet
import Yesod.Form
import Language.Haskell.TH.Syntax
import Yesod.Persist
import Data.Text (Text)
import Web.Routes.Quasi (toSinglePiece, fromSinglePiece)

-- | An entity which can be displayed by the Crud subsite.
class Item a where
    -- | The title of an entity, to be displayed in the list of all entities.
    itemTitle :: a -> Text

-- | Defines all of the CRUD operations (Create, Read, Update, Delete)
-- necessary to implement this subsite. When using the "Yesod.Form" module and
-- 'ToForm' typeclass, you can probably just use 'defaultCrud'.
data Crud master item = Crud
    { crudSelect :: GHandler (Crud master item) master [(Key item, item)]
    , crudReplace :: Key item -> item -> GHandler (Crud master item) master ()
    , crudInsert :: item -> GHandler (Crud master item) master (Key item)
    , crudGet :: Key item -> GHandler (Crud master item) master (Maybe item)
    , crudDelete :: Key item -> GHandler (Crud master item) master ()
    }

mkYesodSub "Crud master item"
    [ ClassP ''Item [VarT $ mkName "item"]
    , ClassP ''SinglePiece [ConT ''Key `AppT` VarT (mkName "item")]
    , ClassP ''ToForm [VarT $ mkName "item", VarT $ mkName "master"]
    ] [parseRoutes|
/               CrudListR        GET
/add            CrudAddR         GET POST
/edit/#Text     CrudEditR        GET POST
/delete/#Text   CrudDeleteR      GET POST
|]

getCrudListR :: (Yesod master, Item item, SinglePiece (Key item))
             => GHandler (Crud master item) master RepHtml
getCrudListR = do
    items <- getYesodSub >>= crudSelect
    toMaster <- getRouteToMaster
    defaultLayout $ do
        setTitle "Items"
        addWidget [hamlet|
<h1>Items
<ul>
    $forall item <- items
        <li>
            <a href="@{toMaster (CrudEditR (toSinglePiece (fst item)))}">
                \#{itemTitle (snd item)}
<p>
    <a href="@{toMaster CrudAddR}">Add new item
|]

getCrudAddR :: (Yesod master, Item item, SinglePiece (Key item),
                ToForm item master)
            => GHandler (Crud master item) master RepHtml
getCrudAddR = crudHelper
                "Add new"
                (Nothing :: Maybe (Key item, item))
                False

postCrudAddR :: (Yesod master, Item item, SinglePiece (Key item),
                 ToForm item master)
             => GHandler (Crud master item) master RepHtml
postCrudAddR = crudHelper
                "Add new"
                (Nothing :: Maybe (Key item, item))
                True

getCrudEditR :: (Yesod master, Item item, SinglePiece (Key item),
                 ToForm item master)
             => Text -> GHandler (Crud master item) master RepHtml
getCrudEditR s = do
    itemId <- maybe notFound return $ fromSinglePiece s
    crud <- getYesodSub
    item <- crudGet crud itemId >>= maybe notFound return
    crudHelper
        "Edit item"
        (Just (itemId, item))
        False

postCrudEditR :: (Yesod master, Item item, SinglePiece (Key item),
                  ToForm item master)
              => Text -> GHandler (Crud master item) master RepHtml
postCrudEditR s = do
    itemId <- maybe notFound return $ fromSinglePiece s
    crud <- getYesodSub
    item <- crudGet crud itemId >>= maybe notFound return
    crudHelper
        "Edit item"
        (Just (itemId, item))
        True

getCrudDeleteR :: (Yesod master, Item item, SinglePiece (Key item))
               => Text -> GHandler (Crud master item) master RepHtml
getCrudDeleteR s = do
    itemId <- maybe notFound return $ fromSinglePiece s
    crud <- getYesodSub
    item <- crudGet crud itemId >>= maybe notFound return -- Just ensure it exists
    toMaster <- getRouteToMaster
    defaultLayout $ do
        setTitle "Confirm delete"
        addWidget [hamlet|
<form method="post" action="@{toMaster (CrudDeleteR s)}">
    <h1>Really delete?
    <p>Do you really want to delete #{itemTitle item}?
    <p>
        <input type="submit" value="Yes">
        \
        <a href="@{toMaster CrudListR}">No
|]

postCrudDeleteR :: (Yesod master, Item item, SinglePiece (Key item))
                => Text -> GHandler (Crud master item) master RepHtml
postCrudDeleteR s = do
    itemId <- maybe notFound return $ fromSinglePiece s
    crud <- getYesodSub
    toMaster <- getRouteToMaster
    crudDelete crud itemId
    redirect RedirectTemporary $ toMaster CrudListR

crudHelper
    :: (Item a, Yesod master, SinglePiece (Key a), ToForm a master)
    => Text -> Maybe (Key a, a) -> Bool
    -> GHandler (Crud master a) master RepHtml
crudHelper title me isPost = do
    crud <- getYesodSub
    (errs, form, enctype, hidden) <- runFormPost $ toForm $ fmap snd me
    toMaster <- getRouteToMaster
    case (isPost, errs) of
        (True, FormSuccess a) -> do
            eid <- case me of
                    Just (eid, _) -> do
                        crudReplace crud eid a
                        return eid
                    Nothing -> crudInsert crud a
            redirect RedirectTemporary $ toMaster $ CrudEditR
                                       $ toSinglePiece eid
        _ -> return ()
    defaultLayout $ do
        setTitle $ toHtml title
        addWidget [hamlet|
<p>
    <a href="@{toMaster CrudListR}">Return to list
<h1>#{title}
<form method="post" enctype="#{enctype}">
    <table>
        \^{form}
        <tr>
            <td colspan="2">
                \#{hidden}
                <input type="submit">
                $maybe e <- me
                    \
                    <a href="@{toMaster (CrudDeleteR (toSinglePiece (fst e)))}">Delete
|]

-- | A default 'Crud' value which relies about persistent and "Yesod.Form".
defaultCrud
    :: (PersistEntity i, PersistBackend (YesodDB a (GGHandler (Crud a i) a IO)),
        YesodPersist a)
    => a -> Crud a i
defaultCrud = const Crud
    { crudSelect = runDB $ selectList [] [] 0 0
    , crudReplace = \a -> runDB . replace a
    , crudInsert = runDB . insert
    , crudGet = runDB . get
    , crudDelete = runDB . delete
    }
