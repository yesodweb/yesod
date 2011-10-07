Title: Experimental, optimized route dispatch code

Let's start with our module declaration and imports.

> module Yesod.Routes.Dispatch
>     ( Piece (..)
>     , Route (..)
>     , Dispatch
>     , toDispatch
>     ) where
>
> import Data.Text (Text)
> import qualified Data.Vector as V
> import Data.Maybe (fromMaybe, mapMaybe)
> import qualified Data.Map as Map
> import Data.List (sortBy)
> import Data.Ord (comparing)
> import Control.Arrow (second)
> import Control.Exception (assert)

This module provides an efficient routing system. The code is pure, requires no
fancy extensions, has no Template Haskell involved and is not Yesod specific.
It does, however, assume a routing system similar to that of Yesod.

Routing works based on splitting up a path into its components. This is handled
very well by both the web-routes and http-types packages, and this module does
not duplicate that functionality. Instead, it assumes that the requested path
will be provided as a list of 'Text's.

A route will be specified by a list of pieces (using the 'Piece' datatype).

> data Piece = Static Text | Dynamic

Each piece is either a static piece- which is required to match a component of
the path precisely- or a dynamic piece, which will match any component.
Additionally, a route can optionally match all remaining components in the
path, or fail if extra components exist.

Usually, the behavior of dynamic is not what you really want. Often times, you
will want to match integers, or slugs, or some other limited format. This
brings us nicely to the dispatch function. Each route provides a function of
type:

> type Dispatch req res = [Text] -> req -> Maybe res

The req and res arguments are application-specific. For example, in a simple
WAI application, they could be the Request and Respone datatypes. The important
thing to point out about Dispatch is that is takes a list of 'Text's and
returns its response in a Maybe. This gives you a chance to having
finer-grained control over how individual components are parsed. If you don't
want to deal with it, you return 'Nothing' and routing continues.

Note: You do *not* need to perform any checking on your static pieces, this
module handles that for you automatically.

So each route is specified by:

> data Route req res = Route
>     { rhPieces :: [Piece]
>     , rhHasMulti :: Bool
>     , rhDispatch :: Dispatch req res
>     }

Your application needs to provide this moudle with a list of routes, and then
this module will give you back a new dispatch function. In other words:

> toDispatch :: [Route req res] -> Dispatch req res
> toDispatch rhs =
>     bcToDispatch bc
>   where
>     bc = toBC rhs

In addition to the requirements listed above for routing, we add one extra
rule: your specified list of routes is treated as ordered, with the earlier
ones matching first. If you have an overlap between two routes, the first one
will be dispatched.

The simplest approach would be to loop through all of your routes and compare
against the path components. But this has linear complexity. Many existing
frameworks (Rails and Django at least) have such algorithms, usually based on
regular expressions. But we can provide two optimizations:

* Break up routes based on how many components they can match. We can then
  select which group of routes to continue testing. This lookup runs in
  constant time.

* Use a Map to reduce string comparisons for each route to logarithmic
  complexity.

Let's start with the first one. Each route has a fixed number of pieces. Let's
call this *n*. If that route can also match trailing components (rhHasMulti
above), then it will match *n* and up. Otherwise, it will match specifically on
*n*.

If *max(n)* is the maximum value of *n* for all routes, what we need is
(*max(n)* + 2) groups: a zero group (matching a request for the root of the
application), 1 - *max(n)* groups, and a final extra group containing all
routes that can match more than *max(n)* components. This group will consist of
all the routes with rhHasMulti, and only those routes.

> data ByCount req res = ByCount
>     { bcVector :: !(V.Vector (PieceMap req res))
>     , bcRest :: !(PieceMap req res)
>     }

We haven't covered PieceMap yet; it is used for the second optimization. We'll
discuss it below.

The following function breaks up a list of routes into groups. Again, please
ignore the PieceMap references for the moment.

> toBC :: [Route req res] -> ByCount req res
> toBC rhs =
>     ByCount
>         { bcVector = groups
>         , bcRest = allMultis
>         }
>   where

Determine the value of *max(n)*.

>     maxLen
>       | null rhs = 0
>       | otherwise = maximum $ map (length . rhPieces) rhs

Get the list of all routes which can have multis. This will make up the *rest*
group.

>     allMultis = toPieceMap maxLen $ filter rhHasMulti rhs

And now get all the numbered groups. For each group, we need to get all routes
with *n* components, __and__ all routes with less than *n* components and that
have rhHasMulti set to True.

>     groups = V.map group $ V.enumFromN 0 (maxLen + 1)
>     group i = toPieceMap i $ filter (canHaveLength i) rhs
>
>     canHaveLength :: Int -> Route req res -> Bool
>     canHaveLength i rh =
>         len == i || (len < i && rhHasMulti rh)
>       where
>         len = length $ rhPieces rh

Next we'll set up our routing by maps. What we need is a bunch of nested Maps.
For example, if we have the following routings:

    /foo/bar/1
    /foo/baz/2

We would want something that looks vaguely like:

    /foo
        /bar
            /1
        /baz
            /2

But there's an added complication: we need to deal with dynamic compnents and HasMulti as well. So what we'd really have is routes looking like:

    /foo/bar/1
    /foo/baz/2
    /*dynamic*/bin/3
    /multi/*bunch of multis*

We can actually simplify away the multi business. Remember that for each group,
we will have a fixed number of components to match. In the list above, it's
three. Even though the last route only has one component, we can actually just
fill up the missing components with *dynamic*, which will give the same result
for routing. In other words, we'll treat it as:

    /foo
        /bar
            /1
        /baz
            /2
    /*dynamic*
        /bin
            /3
    /multi
        /*dynamic*
            /*dynamic*

What we need is then two extra features on our datatype:

* Support both a 'Map Text PieceMap' for static pieces, and a general
  'PieceMap' for all dynamic pieces.

* An extra constructive after we've gone three levels deep, to provide all
  matching routes.

What we end up with is:

> data PieceMap req res = PieceMap
>     { pmDynamic :: PieceMap req res
>     , pmStatic :: Map.Map Text (PieceMap req res)
>     } | PieceMapEnd [(Int, Dispatch req res)]

Note that the PieceMapEnd is a list of pairs, including an Int. Since the map
process will confuse the original order of our routes, we need some way to get
that back to make sure overlapping is handled correctly.

We'll need two pieces of information to make a PieceMap: the depth to drill
down to, and the routes in the current group. We'll immediately zip up those
routes with an Int to indicate route priority.

> toPieceMap :: Int -> [Route req res] -> PieceMap req res
> toPieceMap depth = toPieceMap' depth . zip [1..]
>
> toPieceMap' :: Int
>             -> [(Int, Route req res)]
>             -> PieceMap req res

The stopping case: we've exhausted the full depth, so let's put together a
PieceMapEnd. Technically speaking, the sort here is unnecessary, since we'll
sort again later. However, that second sorting occurs during each dispatch
occurrence, whereas this sorting only occurs once, in the initial construction
of the PieceMap. Therefore, we presort here.

> toPieceMap' 0 rhs =
>     PieceMapEnd $ map (second rhDispatch)
>                 $ sortBy (comparing fst) rhs

Note also that we apply rhDispatch to the route. We are no longer interested in
the rest of the route information, so it can be discarded.

Now the heart of this algorithm: we construct the pmDynamic and pmStatic
records. For both, we recursively call toPieceMap' again, with the depth
knocked down by 1.

> toPieceMap' depth rhs = PieceMap
>     { pmDynamic = toPieceMap' depth' dynamics
>     , pmStatic = Map.map (toPieceMap' depth') statics
>     }
>   where
>     depth' = depth - 1

We turn our list of routes into a list of pairs. The first item in the pair
gives the next piece, and the second gives the route again, minus that piece.

>     pairs = map toPair rhs
>     toPair (i, Route (p:ps) b c) = (p, (i, Route ps b c))

And as we mentioned above, for multi pieces we fill in the remaining pieces
with Dynamic.

>     toPair (i, Route [] b c) = assert b (Dynamic, (i, Route [] b c))

Next, we break up our list of dynamics.

>     getDynamic (Dynamic, rh) = Just rh
>     getDynamic _ = Nothing
>     dynamics = mapMaybe getDynamic pairs

And now we make a Map for statics. Note that Map.fromList would not be
appropriate here, since it would only keep one route per Text.

>     getStatic (Static t, rh) = Just $ Map.singleton t [rh]
>     getStatic _ = Nothing
>     statics = Map.unionsWith (++) $ mapMaybe getStatic pairs

The time has come to actually dispatch.

> bcToDispatch :: ByCount req res -> Dispatch req res
> bcToDispatch (ByCount vec rest) ts0 req =
>     bcToDispatch' ts0 pm0
>   where

Get the PieceMap for the appropriate group. If the length of the requested path
is greater than *max(n)*, then use the "rest" group.

>     pm0 = fromMaybe rest $ vec V.!? length ts0

Stopping case: we've found our list of routes. Sort them, then starting
applying their dispatch functions. If the first one returns Nothing, go to the
next, and so on.

>     bcToDispatch' _ (PieceMapEnd r) = firstJust (\f -> f ts0 req) $ map snd r

For each component, get the static PieceMap and the dynamic one, combine them
together, and then continue dispatching.

>     bcToDispatch' (t:ts) (PieceMap dyn sta) = bcToDispatch' ts $
>         case Map.lookup t sta of
>             Nothing -> dyn
>             Just pm -> append dyn pm

Handle an impossible case that should never happen.

>     bcToDispatch' [] _ = assert False Nothing

Helper function: get the first Just response.

> firstJust :: (a -> Maybe b) -> [a] -> Maybe b
> firstJust _ [] = Nothing
> firstJust f (a:as) = maybe (firstJust f as) Just $ f a

Combine two PieceMaps together.

> append :: PieceMap a b -> PieceMap a b -> PieceMap a b

At the end, just combine the list of routes. But we combine them in such a way
so as to preserve their order. Since a and b come presorted (as mentioned
above), we can just merge the two lists together in linear time.

> append (PieceMapEnd a) (PieceMapEnd b) = PieceMapEnd $ merge a b

Combine the dynamic and static portions of the maps.

> append (PieceMap a x) (PieceMap b y) =
>     PieceMap (append a b) (Map.unionWith append x y)

An impossible case.

> append _ _ = assert False $ PieceMapEnd []

Our O(n) merge.

> merge :: Ord a => [(a, b)] -> [(a, b)] -> [(a, b)]
> merge x [] = x
> merge [] y = y
> merge x@(a@(ai, _):xs) y@(b@(bi, _):ys)
>   | ai < bi   = a : merge xs y
>   | otherwise = b : merge x ys
