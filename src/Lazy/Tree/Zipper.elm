module Lazy.Tree.Zipper
    exposing
        ( BreadCrumb
        , Zipper
        , attempt
        , attemptOpenPath
        , breadCrumbs
        , children
        , current
        , delete
        , fromTree
        , insert
        , isRoot
        , map
        , open
        , openPath
        , root
        , setTree
        , up
        , update
        , upwards
        )

{-| Zipper implementation for Lazy Rose Tree


# Types

@docs BreadCrumb, Zipper, fromTree


# Query

@docs current, children, isRoot, insert, delete, update, setTree, attempt


# Operations

@docs map, open, openPath, attemptOpenPath, up, upwards, root


# BreadCrumbs

@docs breadCrumbs

-}

import Lazy.LList as LL exposing (LList)
import Lazy.Tree as Tree exposing (Forest, Tree)


{-| -}
type alias BreadCrumb a =
    ( Forest a, a, Forest a )


{-| -}
type alias Zipper a =
    ( Tree a, List (BreadCrumb a) )


{-| Init Zipper for Tree

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> current
    --> "foo"

-}
fromTree : Tree a -> Zipper a
fromTree tree =
    ( tree, [] )


{-| Get current Tree

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> current
    --> "foo"

-}
current : Zipper a -> a
current =
    Tree.item << Tuple.first


{-| Get Children of current Tree

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> children
    --> [ "bar" ]

-}
children : Zipper a -> List a
children =
    Tree.children << Tuple.first


{-| Detect if zipper is focused on root tree

    import Lazy.Tree as T

    T.singleton "foo"
       |> fromTree
       |> isRoot
    --> True

-}
isRoot : Zipper a -> Bool
isRoot =
    List.isEmpty << Tuple.second


{-| Insert sub Tree to current Tree

    import Lazy.Tree as T
    import Lazy.LList as LL

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> insert (T.singleton "baz")
        |> children
    --> [ "bar", "baz" ]

-}
insert : Tree a -> Zipper a -> Zipper a
insert tree ( t, breadcrumbs ) =
    ( Tree.insert tree t, breadcrumbs )


{-| Delete Current Tree from Zipper

Returns Nothing if root node is removed.

    import Lazy.Tree as T
    import Lazy.LList as LL

    T.singleton "foo"
        |> fromTree
        |> delete
    --> Nothing

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> open (always True)
        |> Maybe.andThen delete
        |> Maybe.map current
    --> Just "foo"

-}
delete : Zipper a -> Maybe (Zipper a)
delete ( tree, breadcrumbs ) =
    case breadcrumbs of
        [] ->
            Nothing

        ( left, parent, right ) :: tail ->
            Just ( Tree.tree parent (LL.append left right), tail )


{-| Replace Current tree with new one

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> setTree (T.singleton "bar")
        |> current
    --> "bar"

-}
setTree : Tree a -> Zipper a -> Zipper a
setTree tree ( _, breadcrumbs ) =
    ( tree, breadcrumbs )


{-| Update Current Tree using given function

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> update (T.map (\a -> a ++ " fighter"))
        |> current
    --> "foo fighter"

-}
update : (Tree a -> Tree a) -> Zipper a -> Zipper a
update =
    Tuple.mapFirst


{-| Map function over Zipper

    import Lazy.Tree as T

    T.singleton 1
        |> fromTree
        |> map ((+) 1)
        |> current
    --> 2

-}
map : (a -> b) -> Zipper a -> Zipper b
map predicate ( tree, breadcrumbs ) =
    ( Tree.map predicate tree, breadCrumbsMap predicate breadcrumbs )


{-| Attempt to perform action over zipper and return original zipper in cases where this action isn't valid.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> attempt delete
        |> current
    --> "foo"

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> attempt (open ((==) "foo"))
        |> attempt delete
        |> current
    --> "foo"

-}
attempt : (Zipper a -> Maybe (Zipper a)) -> Zipper a -> Zipper a
attempt action zipper =
    Maybe.withDefault zipper <| action zipper


{-| Return back to parent

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> open ((==) "bar")
        |> Maybe.andThen up
        |> Maybe.map current
    --> Just "foo"

    T.singleton "baz"
        |> fromTree
        |> up
    --> Nothing

-}
up : Zipper a -> Maybe (Zipper a)
up ( item, breadcrumbs ) =
    case breadcrumbs of
        [] ->
            Nothing

        ( left, parent, right ) :: tail ->
            Just ( Tree.tree parent (LL.append left (LL.cons item right)), tail )


{-| Go upwards n times.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar" |> T.insert (T.singleton "baz") )
        |> open ((==) "bar")
        |> Maybe.andThen (open ((==) "baz"))
        |> Maybe.andThen (upwards 2)
        |> Maybe.map current
    --> Just "foo"

    T.singleton "foo"
       |> fromTree
       |> upwards 0
       |> Maybe.map current
    --> Just "foo"

    T.singleton 4
        |> fromTree
        |> upwards 1
    --> Nothing

    T.singleton 4
        |> fromTree
        |> upwards -1
    --> Nothing

-}
upwards : Int -> Zipper a -> Maybe (Zipper a)
upwards n zipper =
    if n < 0 then
        Nothing
    else if n == 0 then
        Just zipper
    else
        up zipper
            |> Maybe.andThen (upwards (n - 1))


{-| Back to root Tree

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar" |> T.insert (T.singleton "baz") )
        |> open ((==) "bar")
        |> Maybe.andThen (open ((==) "baz"))
        |> Maybe.map root
        |> Maybe.map current
    --> Just "foo"

    T.singleton "foo"
        |> fromTree
        |> root
        |> current
    --> "foo"

-}
root : Zipper a -> Zipper a
root (( _, breadcrumbs ) as zipper) =
    attempt (upwards <| List.length breadcrumbs) zipper


{-| Oper first parent which satisfy given condition

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> open ((==) "bar")
        |> Maybe.map current
    --> Just "bar"

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar" |> T.insert (T.singleton "baz") )
        |> attempt (open ((==) "bar"))
        |> attempt (open ((==) "baz"))
        |> current
    --> "baz"

    T.singleton "foo"
        |> fromTree
        |> open (always True)
    --> Nothing

-}
open : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
open predicate ( tree, breadcrumbs ) =
    let
        current =
            Tree.item tree

        children =
            Tree.descendants tree

        ( left, item, right ) =
            cutForest predicate children
    in
    Maybe.map (\tree -> ( tree, ( left, current, right ) :: breadcrumbs )) item


{-| Open multiple by reducing list

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar" |> T.insert (T.singleton "baz") )
        |> openPath (==) [ "bar", "baz" ]
        |> Maybe.map current
    --> Just "baz"


    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> openPath (==) [ "not-here", "baz" ]
        |> Maybe.map current
    --> Nothing

-}
openPath : (b -> a -> Bool) -> List b -> Zipper a -> Maybe (Zipper a)
openPath predicate path zipper =
    List.foldl (\i -> Maybe.andThen (open <| predicate i)) (Just zipper) path


{-| Open multiple by reducing list and ignore missmatches.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> attemptOpenPath (==) [ "not-here", "bar" ]
        |> current
    --> "bar"

    T.singleton "foo"
        |> fromTree
        |> attemptOpenPath (==) [ "baz" ]
        |> current
    --> "foo"

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar" |> T.insert (T.singleton "baz"))
        |> attemptOpenPath (==) [ "not-here", "bar", "missng", "baz" ]
        |> current
    --> "baz"

-}
attemptOpenPath : (b -> a -> Bool) -> List b -> Zipper a -> Zipper a
attemptOpenPath predicate path zipper =
    List.foldl (attempt << open << predicate) zipper path


{-| Get breacrubs as indexed list

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar" |> T.insert (T.singleton "baz"))
        |> attemptOpenPath (==) [ "bar", "baz" ]
        |> breadCrumbs
    --> [ ( 1, "bar" ), ( 2, "foo" )]

-}
breadCrumbs : Zipper a -> List ( Int, a )
breadCrumbs =
    List.indexedMap (\i ( _, b, _ ) -> ( i + 1, b )) << Tuple.second



-- Private


breadCrumbsMap : (a -> b) -> List (BreadCrumb a) -> List (BreadCrumb b)
breadCrumbsMap predicate =
    List.map (\( pre, item, after ) -> ( Tree.forestMap predicate pre, predicate item, Tree.forestMap predicate after ))



-- Tree helpers


cutForest_ : Forest a -> (a -> Bool) -> Forest a -> ( Forest a, Maybe (Tree a), Forest a )
cutForest_ acc predicate forest =
    case LL.toList forest of
        [] ->
            ( acc, Nothing, LL.empty )

        head :: tail ->
            if predicate <| Tree.item head then
                ( acc, Just head, LL.fromList tail )
            else
                cutForest_ (LL.cons head acc) predicate (LL.fromList tail)


cutForest : (a -> Bool) -> Forest a -> ( Forest a, Maybe (Tree a), Forest a )
cutForest =
    cutForest_ LL.empty
