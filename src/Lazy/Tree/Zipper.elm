module Lazy.Tree.Zipper exposing
    ( Breadcrumb, Zipper(..), fromTree
    , current, children, isRoot, isEmpty, attempt, getTree, getPath
    , insert, delete, filter, update, updateItem, setTree
    , root, up, upwards, toLeft, toRight, open, openAll, openPath, attemptOpenPath
    , map, duplicate, extend
    , breadcrumbs, indexedBreadcrumbs
    )

{-| Zipper implementation for `Lazy.Tree`.

> A zipper is a technique of representing an aggregate data structure so that it is convenient
> for writing programs that traverse the structure arbitrarily and update its contents,
> especially in purely functional programming languages.

`Zipper` is a secret sauce that gives `Tree` real power.
It provides an easy way to query and modify the `Tree` in a clever and very flexible way.

Types within this module are exposed type aliases to make it easy to extend the default functionality of `Zipper`.


# Types

@docs Breadcrumb, Zipper, fromTree


# Query

@docs current, children, isRoot, isEmpty, attempt, getTree, getPath


# Operations

@docs insert, delete, filter, update, updateItem, setTree


# Navigation

@docs root, up, upwards, toLeft, toRight, open, openAll, openPath, attemptOpenPath


# Transformations

@docs map, duplicate, extend


# Breadcrumbs

@docs breadcrumbs, indexedBreadcrumbs

-}

import Lazy.LList as LL
import Lazy.Tree as Tree exposing (Forest, Tree(..))


{-| \*\* Be careful when comparing `Breadcrumb`s using `(==)`.\*\*
Due to use of laziness `(==)` isn't reliable for comparing Breadcrumbs.

Breadcrumbs are private type not meant to be manipulated directly.
However it's possible to extract breadcrumbs from `Zipper` in transformed
format using `breadcrumbs` and `indexedBreadcrumbs` functions which are meant for public use.

-}
type Breadcrumb a
    = Breadcrumb
        { left : Forest a
        , parent : a
        , right : Forest a
        }


{-| -}
type Zipper a
    = Zipper (Tree a) (List (Breadcrumb a))


{-| Init `Zipper` for `Tree`.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> current
    --> "foo"

-}
fromTree : Tree a -> Zipper a
fromTree tree =
    Zipper tree []


{-| Extract current `Tree` from a `Zipper`.

useful in case where you don't want to use pattern matching

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> getTree
        |> T.item
    --> "foo"

-}
getTree : Zipper a -> Tree a
getTree (Zipper tree _) =
    tree


{-| Get the root item of the current `Tree`.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> current
    --> "foo"

-}
current : Zipper a -> a
current =
    Tree.item << getTree


{-| Get children of current `Tree`.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> children
    --> [ "bar" ]

-}
children : Zipper a -> List a
children =
    Tree.children << getTree


{-| Check if `Zipper` is focused on the root `Tree`.

    import Lazy.Tree as T

    T.singleton "foo"
       |> fromTree
       |> isRoot
    --> True

-}
isRoot : Zipper a -> Bool
isRoot (Zipper _ bs) =
    List.isEmpty bs


{-| Check if current `Tree` in `Zipper` is empty.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> isEmpty
    --> True

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> isEmpty
    --> False

-}
isEmpty : Zipper a -> Bool
isEmpty (Zipper tree _) =
    Tree.isEmpty tree


{-| Insert sub `Tree` into current `Tree` in `Zipper`.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> insert (T.singleton "baz")
        |> children
    --> [ "bar", "baz" ]

-}
insert : Tree a -> Zipper a -> Zipper a
insert tree (Zipper t zipperBreadcrumbs) =
    Zipper (Tree.insert tree t) zipperBreadcrumbs


{-| Delete current `Tree` from `Zipper`.

Returns Nothing if root node is removed.

    import Lazy.Tree as T

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
delete (Zipper _ zipperBreadcrumbs) =
    case zipperBreadcrumbs of
        [] ->
            Nothing

        (Breadcrumb { left, parent, right }) :: tail ->
            Just <| Zipper (Tree parent <| LL.append left right) tail


{-| Replace current `Tree` with new one.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> setTree (T.singleton "bar")
        |> current
    --> "bar"

-}
setTree : Tree a -> Zipper a -> Zipper a
setTree tree (Zipper _ zipperBreadcrumbs) =
    Zipper tree zipperBreadcrumbs


{-| Update current `Tree` using given function.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> update (T.map (\a -> a ++ " fighter"))
        |> current
    --> "foo fighter"

-}
update : (Tree a -> Tree a) -> Zipper a -> Zipper a
update f (Zipper t bs) =
    Zipper (f t) bs


{-| Update the root item of the current `Tree` using given function.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> updateItem (\i -> i ++ " fighter")
        |> current
    --> "foo fighter"

-}
updateItem : (a -> a) -> Zipper a -> Zipper a
updateItem fc (Zipper tree zipperBreadcrumbs) =
    Zipper (Tree (fc <| Tree.item tree) <| Tree.descendants tree) zipperBreadcrumbs


{-| Map function over `Zipper`.

    import Lazy.Tree as T

    T.singleton 1
        |> fromTree
        |> map ((+) 1)
        |> current
    --> 2

-}
map : (a -> b) -> Zipper a -> Zipper b
map fc (Zipper tree zipperBreadcrumbs) =
    Zipper (Tree.map fc tree) <| breadcrumbsMap fc zipperBreadcrumbs


{-| Performs filter on current `Tree` in `Zipper`. See `Tree.filter` for more information.

    import Lazy.LList as LL
    import Lazy.Tree as T

    T.Tree 1 (LL.fromList [ T.singleton 2, T.singleton 3, T.singleton 4 ])
        |> fromTree
        |> filter ((>) 4)
        |> children
    --> [ 2, 3 ]

    T.Tree 1 (LL.fromList [ T.singleton 2, T.singleton 3, T.singleton 4 ])
        |> fromTree
        |> attempt (open ((==) 1))
        |> filter ((<) 2)
        |> root
        |> children
    --> [ 3, 4 ]

    T.Tree 1 (LL.fromList [ T.insert (T.singleton 5) <| T.singleton 2, T.insert (T.singleton 6) <| T.singleton 3, T.singleton 4 ])
        |> fromTree
        |> attempt (open ((==) 1))
        |> filter ((<) 2)
        |> getTree
        |> T.descendants
        |> LL.andThen (LL.map T.item << T.descendants)
        |> LL.toList
    --> [ 6 ]

-}
filter : (a -> Bool) -> Zipper a -> Zipper a
filter predicate (Zipper t bs) =
    Zipper (Tree.filter predicate t) bs


{-| Attempt to perform action over zipper and return original `Zipper` in cases where this action returns `Nothing`.

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


{-| Return back to parent of current `Tree` in given `Zipper`.

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
up (Zipper tree zipperBreadcrumbs) =
    case zipperBreadcrumbs of
        [] ->
            Nothing

        (Breadcrumb { left, parent, right }) :: tail ->
            Just <|
                Zipper
                    (Tree parent <|
                        LL.append (LL.reverse left) <|
                            LL.cons tree right
                    )
                    tail


{-| Move to the immediate left sibling, if it exists.
-}
toLeft : Zipper a -> Maybe (Zipper a)
toLeft (Zipper tree bs) =
    case bs of
        (Breadcrumb { left, parent, right }) :: bs_ ->
            case ( LL.head left, LL.tail left ) of
                ( Just l, Just ls ) ->
                    Just <| Zipper l (Breadcrumb { left = ls, parent = parent, right = LL.cons tree right } :: bs_)

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Move to the immediate right sibling, if it exists.
-}
toRight : Zipper a -> Maybe (Zipper a)
toRight (Zipper tree bs) =
    case bs of
        (Breadcrumb { left, parent, right }) :: bs_ ->
            case ( LL.head right, LL.tail right ) of
                ( Just r, Just rs ) ->
                    Just <| Zipper r (Breadcrumb { left = LL.cons tree left, parent = parent, right = rs } :: bs_)

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Perform [`up`](#up) n times.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar" |> T.insert (T.singleton "baz") )
        |> open ((==) "bar")
        |> Maybe.andThen (open ((==) "baz"))
        |> Maybe.andThen (upwards 2)
        |> Maybe.map current
    --> Just "foo"

Returns given `Zipper` return if `0` is passed:

    T.singleton "foo"
       |> fromTree
       |> upwards 0
       |> Maybe.map current
    --> Just "foo"

Return `Nothing` if there are not enough ancestors in `Zipper`:

    T.singleton 4
        |> fromTree
        |> upwards 1
    --> Nothing

Return `Nothing` if negative integer is passed:

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


{-| Navigate back to the root `Tree`.

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
root ((Zipper _ zipperBreadcrumbs) as zipper) =
    attempt (upwards <| List.length zipperBreadcrumbs) zipper


{-| Open the first child that satisfies given condition.
If there is no child satisfying the condition, returns Nothing.

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
open predicate (Zipper zipperTree zipperBreadcrumbs) =
    let
        currentItem =
            Tree.item zipperTree

        treeChildren =
            Tree.descendants zipperTree

        ( left, item, right ) =
            cutForest predicate treeChildren
    in
    Maybe.map
        (\tree ->
            Zipper tree <|
                Breadcrumb
                    { left = left
                    , parent = currentItem
                    , right = right
                    }
                    :: zipperBreadcrumbs
        )
        item


{-| Use given function to convert current breadcrumb path to a list

Resulting list of breadcrumbs contains currently focused item as well.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> attemptOpenPath (==) [ "bar" ]
        |> getPath identity
    --> [ "foo", "bar" ]

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar" |> T.insert (T.singleton "baz") )
        |> attemptOpenPath (==) [ "bar", "baz" ]
        |> getPath identity
    --> [ "foo", "bar", "baz" ]

-}
getPath : (a -> b) -> Zipper a -> List b
getPath fc (Zipper tree zipperBreadcrumbs) =
    List.foldl (\(Breadcrumb { parent }) acc -> fc parent :: acc)
        [ fc <| Tree.item tree ]
        zipperBreadcrumbs


{-| Open multiple levels reducing list by given function.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar" |> T.insert (T.singleton "baz") )
        |> openPath (==) [ "bar", "baz" ]
        |> Result.map current
    --> Ok "baz"

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> openPath (==) [ "not-here", "baz" ]
        |> Result.map current
    --> Err "Can't resolve open"

-}
openPath : (b -> a -> Bool) -> List b -> Zipper a -> Result String (Zipper a)
openPath predicate path zipper =
    let
        toResult _ =
            Result.fromMaybe "Can't resolve open"
    in
    List.foldl (\i acc -> Result.andThen (toResult i << (open <| predicate i)) acc) (Ok zipper) path


{-| Get `List` of `Zipper`s for all children of current `Zipper`

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar")
        |> insert (T.singleton "baz")
        |> openAll
        |> List.map current
    --> [ "bar", "baz" ]

-}
openAll : Zipper a -> List (Zipper a)
openAll (Zipper tree zipperBreadcrumbs) =
    sliceForest (Tree.descendants tree)
        |> List.map
            (\( left, parent, right ) ->
                Zipper parent <|
                    Breadcrumb
                        { left = left
                        , parent = Tree.item tree
                        , right = right
                        }
                        :: zipperBreadcrumbs
            )


{-| Similar to [`openPath`](#openPath) but ignore failed steps.

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
        |> attemptOpenPath (==) [ "not-here", "bar", "missing", "baz" ]
        |> current
    --> "baz"

-}
attemptOpenPath : (b -> a -> Bool) -> List b -> Zipper a -> Zipper a
attemptOpenPath predicate path zipper =
    List.foldl (attempt << open << predicate) zipper path


{-| Get `List` of `Breadcrumb`s .

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar" |> T.insert (T.singleton "baz"))
        |> attemptOpenPath (==) [ "bar", "baz" ]
        |> breadcrumbs
    --> [ "bar", "foo" ]

-}
breadcrumbs : Zipper a -> List a
breadcrumbs (Zipper _ bs) =
    List.map (\(Breadcrumb { parent }) -> parent) bs


{-| Get `Breadcrumb`s as indexed `List`.

    import Lazy.Tree as T

    T.singleton "foo"
        |> fromTree
        |> insert (T.singleton "bar" |> T.insert (T.singleton "baz"))
        |> attemptOpenPath (==) [ "bar", "baz" ]
        |> indexedBreadcrumbs
    --> [ ( 1, "bar" ), ( 2, "foo" )]

-}
indexedBreadcrumbs : Zipper a -> List ( Int, a )
indexedBreadcrumbs (Zipper _ bs) =
    List.indexedMap (\i (Breadcrumb { parent }) -> ( i + 1, parent )) bs


{-| Duplicate Zipper (Comonad).
Converts each node into its full Zipper context.
For example, the [`extend`] function is defined as

    extend : (Zipper a -> b) -> Zipper a -> Zipper b
    extend f =
        map f << duplicate

-}
duplicate : Zipper a -> Zipper (Zipper a)
duplicate zipper =
    let
        genLeft z =
            case toLeft z of
                Nothing ->
                    LL.empty

                Just zl ->
                    LL.cons (Tree.build openAll zl) <| genLeft zl

        genRight z =
            case toRight z of
                Nothing ->
                    LL.empty

                Just zr ->
                    LL.cons (Tree.build openAll zr) <| genRight zr

        genBreadcrumbs z =
            case up z of
                Nothing ->
                    []

                Just zp ->
                    Breadcrumb { left = genLeft z, parent = zp, right = genRight z } :: genBreadcrumbs zp
    in
    Zipper (Tree.build openAll zipper) <| genBreadcrumbs zipper


{-| Extend Zipper (Comonad).
Sort of like a contextful `map` where instead of mapping only each element,
you have access to the whole Zipper at each element's location.
E.g.:

    extendWithPath : Zipper a -> Zipper ( a, List a )
    extendWithPath =
        extend (\z -> ( current z, getPath identity z ))

-}
extend : (Zipper a -> b) -> Zipper a -> Zipper b
extend f =
    map f << duplicate



-- Private


breadcrumbsMap : (a -> b) -> List (Breadcrumb a) -> List (Breadcrumb b)
breadcrumbsMap predicate =
    List.map
        (\(Breadcrumb { left, parent, right }) ->
            Breadcrumb
                { left = Tree.forestMap predicate left
                , parent = predicate parent
                , right = Tree.forestMap predicate right
                }
        )



-- Tree helpers


cutForest : (a -> Bool) -> Forest a -> ( Forest a, Maybe (Tree a), Forest a )
cutForest =
    cutForest_ LL.empty


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


sliceForest : Forest a -> List ( Forest a, Tree a, Forest a )
sliceForest =
    sliceForest_ [] LL.empty


sliceForest_ : List ( Forest a, Tree a, Forest a ) -> Forest a -> Forest a -> List ( Forest a, Tree a, Forest a )
sliceForest_ acc left right =
    case LL.toList right of
        [] ->
            List.reverse acc

        head :: tail ->
            let
                newItem =
                    ( left, head, LL.fromList tail )
            in
            sliceForest_ (newItem :: acc) (LL.cons head left) (LL.fromList tail)
