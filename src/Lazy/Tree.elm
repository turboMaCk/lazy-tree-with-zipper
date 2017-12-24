module Lazy.Tree
    exposing
        ( Forest
        , Tree
        , andMap
        , andThen
        , children
        , flatten
        , forestMap
        , fromList
        , item
        , map
        , map2
        , singleton
        , tree
        )

{-| Lazy Tree with zipper
nanana


# Types

@docs Tree, Forest, tree, singleton, fromList


# Query

@docs item, children


# Transforms

@docs map, map2, andMap, flatten, andThen


# Forest

@docs forestMap

-}

import Lazy.List as LL exposing ((+++), LazyList)


{-| -}
type Tree a
    = Tree a (Forest a)


{-| -}
type alias Forest a =
    LazyList (Tree a)



-- Tree


{-| puts value in minimal tree context

    import Lazy.List as LL

    singleton "foo"
    --> tree "foo" LL.empty

-}
singleton : a -> Tree a
singleton a =
    Tree a LL.empty


{-| Tree constructor

    import Lazy.List as LL

    tree "foo" LL.empty
        |> item
    --> "foo"


    tree "foo" (LL.fromList [ singleton "bar", singleton "baz" ])
        |> children
        |> LL.map item
        |> LL.toList
    --> [ "bar", "baz" ]

-}
tree : a -> Forest a -> Tree a
tree =
    Tree


{-| Obtain item from tree

    singleton "foo"
        |> item
        |> "foo"

-}
item : Tree a -> a
item (Tree i _) =
    i


{-| Obtain children as Forest from the tree

    import Lazy.List as LL

    tree "foo" (LL.fromList [ singleton "bar", singleton "baz" ])
        |> children
        |> LL.toList
    --> [ singleton "bar", singleton "baz" ]

-}
children : Tree a -> Forest a
children (Tree _ c) =
    c


{-| Map function over tree

    singleton 1
        |> map ((+) 1)
        |> item
    --> 2

    tree 1 (LL.fromList [ singleton 2, singleton 3 ])
        |> map ((*) 2)
        |> children
        |> LL.toList
        |> List.map item
    --> [ 4, 6 ]

** Be careful when comparing mapped Trees using `(==)`.**
Due to use of `LazyList` and lack of type classes in Elm `(==)` isn't reliable
for comparing Trees.

-}
map : (a -> b) -> Tree a -> Tree b
map predicate (Tree a forest) =
    tree (predicate a) <| forestMap predicate forest


{-| Map function over two trees

    import Lazy.List as LL

    map2 (+) (singleton 1) (singleton 5)
        |> item
    --> 6

    tree 1 (LL.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> map2 (+) (tree 5 <| LL.fromList [ singleton 6, singleton 7 ])
        |> children
        |> LL.map item
        |> LL.toList
    --> [ 8, 10 ]

-}
map2 : (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2 predicate (Tree a1 f1) (Tree a2 f2) =
    tree (predicate a1 a2) <| forestMap2 predicate f1 f2


{-| Chain map operations

    import Lazy.List as LL

    tree (,) (LL.fromList [ singleton (,), singleton (,), singleton (,) ])
        |> andMap (tree 1 <| LL.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> andMap (tree 5 <| LL.fromList [ singleton 6, singleton 7 ])
        |> children
        |> LL.map item
        |> LL.toList
    --> [ (2, 6), (3, 7) ]

-}
andMap : Tree a -> Tree (a -> b) -> Tree b
andMap =
    map2 (|>)


{-| Flatten Tree of Trees

    import Lazy.List as LL

    singleton (singleton 1)
        |> flatten
        |> item
    --> 1

    tree (tree "foo" <| LL.fromList [ singleton "bar"]) (LL.fromList [ singleton <| singleton "baz" ])
        |> flatten
        |> children
        |> LL.toList
        |> List.map item
    --> [ "bar", "baz" ]

-}
flatten : Tree (Tree a) -> Tree a
flatten (Tree (Tree item c) children) =
    tree item <| c +++ LL.map flatten children


{-| Maping tree construction over Tree.

    tree "foo" (LL.fromList [ singleton "bar", singleton "baz" ])
        |> andThen (\a -> tree a <| LL.fromList [ singleton <| a ++ " fighter" ])
        |> children
        |> LL.toList
        |> List.map item
    --> [ "foo fighter", "bar", "baz" ]

-}
andThen : (a -> Tree b) -> Tree a -> Tree b
andThen fc =
    flatten << map fc


{-| -}
fromList : a -> (Maybe a -> a -> Bool) -> List a -> Tree a
fromList root isParent =
    tree root << fromList_ Nothing isParent



-- Forest


{-| -}
forestMap : (a -> b) -> Forest a -> Forest b
forestMap predicate =
    LL.map (map predicate)


{-| -}
forestMap2 : (a -> b -> c) -> Forest a -> Forest b -> Forest c
forestMap2 predicate =
    LL.map2 (map2 predicate)



-- Private


constructTree : (Maybe a -> a -> Bool) -> List a -> a -> Tree a
constructTree isParent list item =
    tree item <| fromList_ (Just item) isParent list


fromList_ : Maybe a -> (Maybe a -> a -> Bool) -> List a -> Forest a
fromList_ parent isParent list =
    LL.fromList <|
        List.map (constructTree isParent list) <|
            List.filter (isParent parent) list
