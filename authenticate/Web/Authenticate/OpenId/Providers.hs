-- | OpenIDs for a number of common OPs. When a function takes a 'String'
-- parameter, that 'String' is the username.
module Web.Authenticate.OpenId.Providers
    ( google
    , yahoo
    , livejournal
    , myspace
    , wordpress
    , blogger
    , verisign
    , typepad
    , myopenid
    , claimid
    ) where

google :: String
google = "https://www.google.com/accounts/o8/id"

yahoo :: String
yahoo = "http://me.yahoo.com/"

livejournal :: String -> String
livejournal u = concat ["http://", u, ".livejournal.com/"]

myspace :: String -> String
myspace = (++) "http://www.myspace.com/"

wordpress :: String -> String
wordpress u = concat ["http://", u, ".wordpress.com/"]

blogger :: String -> String
blogger u = concat ["http://", u, ".blogger.com/"]

verisign :: String -> String
verisign u = concat ["http://", u, ".pip.verisignlabs.com/"]

typepad :: String -> String
typepad u = concat ["http://", u, ".typepad.com/"]

myopenid :: String -> String
myopenid u = concat ["http://", u, ".myopenid.com/"]

claimid :: String -> String
claimid = (++) "http://claimid.com/"
