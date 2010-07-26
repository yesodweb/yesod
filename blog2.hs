{-# LANGUAGE TypeFamilies, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
import Yesod
import Yesod.Helpers.Crud
import Yesod.Form.Jquery
import Yesod.Form.Nic
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time (Day)

share2 mkToForm mkPersist [$persist|
Entry
    title String id=thetitle
    day Day Desc toFormField=YesodJquery.jqueryDayField name=day
    content Html' toFormField=YesodNic.nicHtmlField
    deriving
|]

instance Item Entry where
    itemTitle = entryTitle

data Blog = Blog { pool :: Pool Connection }

type EntryCrud = Crud Blog Entry

mkYesod "Blog" [$parseRoutes|
/ RootR GET
/entry/#EntryId EntryR GET
/admin AdminR EntryCrud defaultCrud
|]

instance Yesod Blog where
    approot _ = "http://localhost:3000"
instance YesodJquery Blog
instance YesodNic Blog

instance YesodPersist Blog where
    type YesodDB Blog = SqliteReader
    runDB db = fmap pool getYesod>>= runSqlite db

getRootR = do
    entries <- runDB $ selectList [] [EntryDayDesc] 0 0
    applyLayoutW $ do
        setTitle $ string "Yesod Blog Tutorial Homepage"
        addBody [$hamlet|
%h1 Archive
%ul
    $forall entries entry
        %li
            %a!href=@EntryR.fst.entry@ $entryTitle.snd.entry$
%p
    %a!href=@AdminR.CrudListR@ Admin
|]

getEntryR entryid = do
    entry <- runDB $ get404 entryid
    applyLayoutW $ do
        setTitle $ string $ entryTitle entry
        addBody [$hamlet|
%h1 $entryTitle.entry$
%h2 $show.entryDay.entry$
$entryContent.entry$
|]

withBlog f = withSqlite ":memory:" 8 $ \p -> do
    flip runSqlite p $ do
        initialize (undefined :: Entry)
    f $ Blog p

main = withBlog $ basicHandler 3000
