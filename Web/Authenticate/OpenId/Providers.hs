module Web.Authenticate.OpenId.Providers
    where

google = "https://www.google.com/accounts/o8/id"
yahoo = "http://me.yahoo.com/"
livejournal u = concat ["http://", u, ".livejournal.com/"]
myspace = (++) "http://myspace.com/"
wordpress u = concat ["http://", u, ".wordpress.com/"]
blogger u = concat ["http://", u, ".blogger.com/"]
verisign u = concat ["http://", u, ".pip.verisignlabs.com/"]
typepad u = concat ["http://", u, ".typepad.com/"]
myopenid u = concat ["http://", u, ".myopenid.com/"]
claimid = (++) "http://claimid.com/"
