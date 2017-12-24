module Lazy.Tree
    exposing
        ( Forest
        , Tree
        , andMap
        , andThen
        , children
        , constructTree
        , flatten
        , forestMap
        , fromList
        , item
        , map
        , map2
        , singleton
        , tree
        )

import Lazy.List as LL exposing (LazyList)


type Tree a
    = Tree a (Forest a)


type alias Forest a =
    LazyList (Tree a)



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
    tree (predicate a) <| forestMap predicate forest


map2 : (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2 predicate (Tree a1 f1) (Tree a2 f2) =
    tree (predicate a1 a2) <| forestMap2 predicate f1 f2


andMap : Tree a -> Tree (a -> b) -> Tree b
andMap =
    map2 (|>)


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


forestMap2 : (a -> b -> c) -> Forest a -> Forest b -> Forest c
forestMap2 predicate =
    LL.map2 (map2 predicate)


fromList_ : Maybe a -> (Maybe a -> a -> Bool) -> List a -> Forest a
fromList_ parent isParent list =
    LL.fromList <|
        List.map (constructTree isParent list) <|
            List.filter (isParent parent) list
