{-# LANGUAGE TypeFamilies, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MkToForm2 where

import Yesod
import Data.Time (Day)

mkPersist [$persist|
Entry
    title String
    day Day Desc toFormField=YesodJquery.jqueryDayField'
    content Html toFormField=YesodNic.nicHtmlField
    deriving
|]
