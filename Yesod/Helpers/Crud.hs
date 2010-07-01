{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
module Yesod.Helpers.Crud
    ( Item (..)
    , Crud (..)
    , CrudRoutes (..)
    , defaultCrud
    ) where

import Yesod.Yesod
import Yesod.Widget
import Yesod.Dispatch
import Yesod.Content
import Yesod.Handler
import Text.Hamlet
import Yesod.Form
import Data.Monoid (mempty)

class Formable a => Item a where
    itemTitle :: a -> String

data Crud master item = Crud
    { crudSelect :: GHandler (Crud master item) master [(Key item, item)]
    , crudReplace :: Key item -> item -> GHandler (Crud master item) master ()
    , crudInsert :: item -> GHandler (Crud master item) master (Key item)
    , crudGet :: Key item -> GHandler (Crud master item) master (Maybe item)
    , crudDelete :: Key item -> GHandler (Crud master item) master ()
    }

mkYesodSub "Crud master item"
    [ ("master", [''Yesod])
    , ("item", [''Item])
    , ("Key item", [''SinglePiece])
    , ("Routes master", [''Eq])
    ] [$parseRoutes|
/               CrudListR        GET
/add            CrudAddR         GET POST
/edit/#String   CrudEditR        GET POST
/delete/#String CrudDeleteR      GET POST
|]

getCrudListR :: (Yesod master, Item item, SinglePiece (Key item))
             => GHandler (Crud master item) master RepHtml
getCrudListR = do
    items <- getYesodSub >>= crudSelect
    toMaster <- getRouteToMaster
    applyLayout "Items" mempty [$hamlet|
%h1 Items
%ul
    $forall items item
        %li
            %a!href=@toMaster.CrudEditR.toSinglePiece.fst.item@
                $string.itemTitle.snd.item$
%p
    %a!href=@toMaster.CrudAddR@ Add new item
|]

getCrudAddR :: (Yesod master, Item item, SinglePiece (Key item),
                Eq (Routes master))
            => GHandler (Crud master item) master RepHtml
getCrudAddR = crudHelper
                "Add new"
                (Nothing :: Maybe (Key item, item))
                False

postCrudAddR :: (Yesod master, Item item, SinglePiece (Key item),
                 Eq (Routes master))
             => GHandler (Crud master item) master RepHtml
postCrudAddR = crudHelper
                "Add new"
                (Nothing :: Maybe (Key item, item))
                True

getCrudEditR :: (Yesod master, Item item, SinglePiece (Key item),
                 Eq (Routes master))
             => String -> GHandler (Crud master item) master RepHtml
getCrudEditR s = do
    itemId <- maybe notFound return $ itemReadId s
    crud <- getYesodSub
    item <- crudGet crud itemId >>= maybe notFound return
    crudHelper
        "Edit item"
        (Just (itemId, item))
        False

postCrudEditR :: (Yesod master, Item item, SinglePiece (Key item),
                  Eq (Routes master))
              => String -> GHandler (Crud master item) master RepHtml
postCrudEditR s = do
    itemId <- maybe notFound return $ itemReadId s
    crud <- getYesodSub
    item <- crudGet crud itemId >>= maybe notFound return
    crudHelper
        "Edit item"
        (Just (itemId, item))
        False

getCrudDeleteR :: (Yesod master, Item item, SinglePiece (Key item))
               => String -> GHandler (Crud master item) master RepHtml
getCrudDeleteR s = do
    itemId <- maybe notFound return $ itemReadId s
    crud <- getYesodSub
    item <- crudGet crud itemId >>= maybe notFound return -- Just ensure it exists
    toMaster <- getRouteToMaster
    applyLayout "Confirm delete" mempty [$hamlet|
%form!method=post!action=@toMaster.CrudDeleteR.s@
    %h1 Really delete?
    %p Do you really want to delete $string.itemTitle.item$?
    %p
        %input!type=submit!value=Yes
        \ $
        %a!href=@toMaster.CrudListR@ No
|]

postCrudDeleteR :: (Yesod master, Item item, SinglePiece (Key item))
                => String -> GHandler (Crud master item) master RepHtml
postCrudDeleteR s = do
    itemId <- maybe notFound return $ itemReadId s
    crud <- getYesodSub
    toMaster <- getRouteToMaster
    crudDelete crud itemId
    redirect RedirectTemporary $ toMaster CrudListR

itemReadId :: SinglePiece x => String -> Maybe x
itemReadId = either (const Nothing) Just . fromSinglePiece

crudHelper
    :: (Item a, Yesod master, SinglePiece (Key a), Eq (Routes master))
    => String -> Maybe (Key a, a) -> Bool
    -> GHandler (Crud master a) master RepHtml
crudHelper title me isPost = do
    crud <- getYesodSub
    (errs, form) <- runFormPost $ formable $ fmap snd me
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
    applyLayoutW $ do
        wrapWidget (wrapForm toMaster) form
        setTitle $ string title
  where
    wrapForm toMaster form = [$hamlet|
%p
    %a!href=@toMaster.CrudListR@ Return to list
%h1 $string.title$
%form!method=post
    %table
        ^form^
        %tr
            %td!colspan=2
                %input!type=submit
                $maybe me e
                    \ 
                    %a!href=@toMaster.CrudDeleteR.toSinglePiece.fst.e@ Delete
|]

defaultCrud
    :: (PersistEntity i, PersistBackend (YesodDB a (GHandler (Crud a i) a)),
        YesodPersist a)
    => a -> Crud a i
defaultCrud = const Crud
    { crudSelect = runDB $ select [] []
    , crudReplace = \a -> runDB . replace a
    , crudInsert = runDB . insert
    , crudGet = runDB . get
    , crudDelete = runDB . delete
    }
