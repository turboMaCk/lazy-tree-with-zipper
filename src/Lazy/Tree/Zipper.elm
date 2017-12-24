module Lazy.Tree.Zipper
    exposing
        ( BreadCrumb
        , Zipper
        , attempt
        , attemptOpenPath
        , breadCrumbsMap
        , current
        , currentChildren
        , fromTree
        , open
        , openPath
        , root
        , up
        , upwards
        )

import Lazy.List as LL exposing ((+++), (:::), LazyList)
import Lazy.Tree as Tree exposing (Forest, Tree)


type alias BreadCrumb a =
    ( Forest a, a, Forest a )


type alias Zipper a =
    ( Tree a, List (BreadCrumb a) )


fromTree : Tree a -> Zipper a
fromTree tree =
    ( tree, [] )


current : Zipper a -> a
current =
    Tree.item << Tuple.first


currentChildren : Zipper a -> LazyList a
currentChildren =
    LL.map Tree.item << Tree.children << Tuple.first


map : (a -> b) -> Zipper a -> Zipper b
map predicate ( tree, breadcrumbs ) =
    ( Tree.map predicate tree, breadCrumbsMap predicate breadcrumbs )


attempt : (Zipper a -> Maybe (Zipper a)) -> Zipper a -> Zipper a
attempt action zipper =
    Maybe.withDefault zipper <| action zipper


up : Zipper a -> Maybe (Zipper a)
up ( item, breadcrumbs ) =
    case breadcrumbs of
        [] ->
            Nothing

        ( left, parent, right ) :: tail ->
            Just ( Tree.tree parent (item ::: left +++ right), tail )


upwards : Int -> Zipper a -> Maybe (Zipper a)
upwards n zipper =
    if n < 0 then
        Nothing
    else if n == 0 then
        Just zipper
    else
        up zipper
            |> Maybe.andThen (upwards (n - 1))


root : Zipper a -> Zipper a
root (( _, breadcrumbs ) as zipper) =
    attempt (upwards <| List.length breadcrumbs) zipper


open : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
open predicate ( tree, breadcrumbs ) =
    let
        current =
            Tree.item tree

        children =
            Tree.children tree

        ( pre, item, after ) =
            cutForest predicate children
    in
    case item of
        Just tree ->
            Just ( tree, ( pre, current, after ) :: breadcrumbs )

        Nothing ->
            Nothing


openPath : (b -> a -> Bool) -> List b -> Zipper a -> Maybe (Zipper a)
openPath predicate path zipper =
    case path of
        [] ->
            Just zipper

        head :: tail ->
            open (predicate head) zipper
                |> Maybe.andThen (openPath predicate tail)


attemptOpenPath : (b -> a -> Bool) -> List b -> Zipper a -> Zipper a
attemptOpenPath predicate path zipper =
    List.foldr (attempt << open << predicate) zipper path


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
                cutForest_ (head ::: acc) predicate (LL.fromList tail)


cutForest : (a -> Bool) -> Forest a -> ( Forest a, Maybe (Tree a), Forest a )
cutForest =
    cutForest_ LL.empty
