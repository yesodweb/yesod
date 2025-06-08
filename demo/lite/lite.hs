{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Yesod.Core
import Data.Aeson
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif
import Data.Text (Text, pack)

people :: [(Text, Int)]
people = [("Alice", 25), ("Bob", 43), ("Charlie", 37)]

main = warp 3000 $ liteApp $ do
    onStatic "people" $ dispatchTo getPeople
    onStatic "person" $ withDynamic $ dispatchTo . getPerson

getPeople = return $ toJSON $ map fst people

getPerson name =
    case lookup name people of
        Nothing -> notFound
        Just age -> selectRep $ do
            provideRep $ return $ object ["name" .= name, "age" .= age]
            provideRep $ return $ name <> " is " <> pack (show age) <> " years old"
