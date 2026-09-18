module MissingPolicy where
import Yesod.Core (Application)
import YesodCoreTest.RouteLeafHook.Account (accountApp)
import YesodCoreTest.RouteLeafHook.Foundation (LeafApp)

-- The dispatch module compiles, but using it without its policy must fail.
application :: LeafApp -> IO Application
application = accountApp
