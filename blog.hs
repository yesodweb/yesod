{-# LANGUAGE TypeFamilies, QuasiQuotes, GeneralizedNewtypeDeriving #-}
import Yesod
import Yesod.Helpers.Auth
import Yesod.Helpers.Crud
import Database.Persist.Sqlite
import Data.Time (Day)

share2 mkPersist mkIsForm [$persist|
Entry
    title String "label=Entry title" "tooltip=Make it something cool"
    posted JqueryDay Desc
    content NicHtml
    deriving
|]
instance Item Entry where
    itemTitle = entryTitle

getAuth = const $ Auth
    { authIsOpenIdEnabled = False
    , authRpxnowApiKey = Nothing
    , authEmailSettings = Nothing
    -- | client id, secret and requested permissions
    , authFacebook = Just (clientId, secret, ["email"])
    }
  where
    clientId = "134280699924829"
    secret = "a7685e10c8977f5435e599aaf1d232eb"

data Blog = Blog Connection
type EntryCrud = Crud Blog Entry
mkYesod "Blog" [$parseRoutes|
/ RootR GET
/entry/#EntryId EntryR GET
/admin AdminR EntryCrud defaultCrud
/auth AuthR Auth getAuth
|]
instance Yesod Blog where
    approot _ = "http://localhost:3000"
    defaultLayout p = do
        mcreds <- maybeCreds
        admin <- maybeAuthorized $ AdminR CrudListR
        hamletToContent [$hamlet|
!!!
%html
    %head
        %title $<pageTitle.p>$
        ^pageHead.p^
        %style textarea.html{width:500px;height:200px}div.tooltip{font-size:80%;font-style:italic;color:#666}
    %body
        %p
            %a!href=@RootR@ Homepage
            $maybe admin a
                \ | $
                %a!href=@a@ Admin
            \ | $
            $maybe mcreds c
                Welcome $
                $maybe credsDisplayName.c dn
                    $dn$
                $nothing
                    $credsIdent.c$
                \ $
                %a!href=@AuthR.Logout@ Logout
            $nothing
                %a!href=@AuthR.StartFacebookR@ Facebook Connect
        ^pageBody.p^
        %p
            Powered by Yesod Web Framework
|]
    isAuthorized AdminR{} = do
        mc <- maybeCreds
        let x = (mc >>= credsEmail) == Just "michael@snoyman.com"
        return $ if x then Nothing else Just "Permission denied"
    isAuthorized _ = return Nothing
instance YesodAuth Blog where
    defaultDest _ = RootR
    defaultLoginRoute _ = RootR
instance YesodPersist Blog where
    type YesodDB Blog = SqliteReader
    runDB db = do
        Blog conn <- getYesod
        runSqlite db conn

getRootR = do
    entries <- runDB $ select [] [EntryPostedDesc]
    applyLayoutW $ do
        setTitle $ string "Blog tutorial homepage"
        addBody [$hamlet|
%h1 All Entries
%ul
    $forall entries entry
        %li
            %a!href=@EntryR.fst.entry@ $entryTitle.snd.entry$
|]

getEntryR :: EntryId -> Handler Blog RepHtml
getEntryR eid = do
    entry <- runDB (get eid) >>= maybe notFound return
    applyLayoutW $ do
        setTitle $ string $ entryTitle entry
        addBody [$hamlet|
%h1 $entryTitle.entry$
%h2 $show.unJqueryDay.entryPosted.entry$
#content $<unNicHtml.entryContent.entry>$
|]
main = withSqlite "blog.db3" $ \conn -> do
    flip runSqlite conn $ initialize (undefined :: Entry)
    toWaiApp (Blog conn) >>= basicHandler 3000
