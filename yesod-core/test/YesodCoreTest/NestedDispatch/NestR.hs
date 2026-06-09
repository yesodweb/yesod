{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Split-out @NestR@ fragment of the "YesodCoreTest.NestedDispatch" demo. The
-- @setFocusOnNestedRoute "NestR"@ splice generates the @NestR@ datatype and its
-- @YesodDispatchNested@ instance here; the @App@ module delegates to it. The
-- single @NestIndexR@ leaf handles both GET and POST, covering a zero-piece
-- nested index reached through delegation.
module YesodCoreTest.NestedDispatch.NestR where

import Yesod.Core
import Data.Text (Text)
import YesodCoreTest.NestedDispatch.Resources

mkYesodOpts (setFocusOnNestedRoute "NestR" defaultOpts) "App" nestedDispatchResources

getNestIndexR :: HandlerFor App Text
getNestIndexR = pure "getNestIndexR"

postNestIndexR :: HandlerFor App String
postNestIndexR = pure "hello"
