module Lazy.Tree exposing (..)

import Lazy.List as LL exposing ((+++), (:::), LazyList)


type Tree a
    = Tree a (Forest a)


type alias Forest a =
    LazyList (Tree a)


type alias BreadCrumb a =
    ( Forest a, a, Forest a )


type alias Zipper a =
    ( Tree a, List (BreadCrumb a) )



-- Tree


singleton : a -> Tree a
singleton a =
    Tree a LL.empty


tree : a -> Forest a -> Tree a
tree =
    Tree


item : Tree a -> a
item (Tree i _) =
    i


children : Tree a -> Forest a
children (Tree _ c) =
    c


map : (a -> b) -> Tree a -> Tree b
map predicate (Tree a forest) =
    Tree (predicate a) <| forestMap predicate forest


flatten : Tree (Tree a) -> Tree a
flatten (Tree inner _) =
    inner


andThen : (a -> Tree b) -> Tree a -> Tree b
andThen fc =
    flatten << map fc


constructTree : (Maybe a -> a -> Bool) -> List a -> a -> Tree a
constructTree isParent list item =
    tree item <| fromList_ (Just item) isParent list


fromList : a -> (Maybe a -> a -> Bool) -> List a -> Tree a
fromList root isParent =
    tree root << fromList_ Nothing isParent



-- Forest


forestMap : (a -> b) -> Forest a -> Forest b
forestMap predicate =
    LL.map (map predicate)


fromList_ : Maybe a -> (Maybe a -> a -> Bool) -> List a -> Forest a
fromList_ parent isParent list =
    LL.fromList <|
        List.map (constructTree isParent list) <|
            List.filter (isParent parent) list


cutForest_ : Forest a -> (a -> Bool) -> Forest a -> ( Forest a, Maybe (Tree a), Forest a )
cutForest_ acc predicate forest =
    case LL.toList forest of
        [] ->
            ( acc, Nothing, LL.empty )

        ((Tree a _) as head) :: tail ->
            if predicate a then
                ( acc, Just head, LL.fromList tail )
            else
                cutForest_ (head ::: acc) predicate (LL.fromList tail)


cutForest : (a -> Bool) -> Forest a -> ( Forest a, Maybe (Tree a), Forest a )
cutForest =
    cutForest_ LL.empty



-- Bread Crumb


toZipper : Tree a -> Zipper a
toZipper tree =
    ( tree, [] )


current : Zipper a -> a
current =
    item << Tuple.first


currentChildren : Zipper a -> LazyList a
currentChildren =
    LL.map item << children << Tuple.first


attempt : (Zipper a -> Maybe (Zipper a)) -> Zipper a -> Zipper a
attempt action zipper =
    Maybe.withDefault zipper <| action zipper


up : Zipper a -> Maybe (Zipper a)
up ( item, breadcrumbs ) =
    case breadcrumbs of
        [] ->
            Nothing

        ( left, parent, right ) :: tail ->
            Just ( tree parent (item ::: left +++ right), tail )


open : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
open predicate ( Tree current children, breadcrumbs ) =
    let
        ( pre, item, after ) =
            cutForest predicate children
    in
    case item of
        Just ((Tree a _) as i) ->
            Just ( i, ( pre, current, after ) :: breadcrumbs )

        Nothing ->
            Nothing
