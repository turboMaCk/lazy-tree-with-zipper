module Lazy.Tree exposing (..)

import Lazy.List as LL exposing ((:::), LazyList)


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
