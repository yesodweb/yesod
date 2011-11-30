This example uses the sqlite backend for Persistent, since it can run in-memory and has no external dependencies.

> {-# LANGUAGE GADTs, TypeFamilies, GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
>
> import Database.Persist.Sqlite
> import Database.Persist.TH
> import Control.Monad.IO.Class (liftIO)
>
> mkPersist sqlSettings [persist|Person
>     name String Eq
>     age Int Update
> |]
>
> main :: IO ()
> main = withSqliteConn ":memory:" $ runSqlConn go
>
> go :: SqlPersist IO ()
> go = do
>   runMigration $ migrate (undefined :: Person)
>   key <- insert $ Person "Michael" 25
>   liftIO $ print key
>   p1 <- get key
>   liftIO $ print p1
>   update key [PersonAge =. 26]
>   p2 <- get key
>   liftIO $ print p2
>   p3 <- selectList [PersonName ==. "Michael"] []
>   liftIO $ print p3
>   delete key
>   p4 <- selectList [PersonName ==. "Michael"] []
>   liftIO $ print p4

The output of the above is:

<code><pre>PersonId 1
Just (Person {personName = "Michael", personAge = 25})
Just (Person {personName = "Michael", personAge = 26})
[(PersonId 1,Person {personName = "Michael", personAge = 26})]
[]</pre></code>

> _ignored :: PersonId
> _ignored = undefined personName personAge
