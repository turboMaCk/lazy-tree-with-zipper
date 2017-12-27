module Lazy.Tree
    exposing
        ( Forest
        , Tree
        , andMap
        , andThen
        , children
        , descendants
        , flatten
        , forestMap
        , forestMap2
        , fromList
        , insert
        , isEmpty
        , item
        , map
        , map2
        , singleton
        , tree
        )

{-| Lazy Rose Tree implementation


# Types

@docs Tree, Forest, tree, singleton, insert, fromList


# Query

@docs isEmpty, item, children, descendants


# Transforms

@docs map, map2, andMap, flatten, andThen


# Forest

@docs forestMap, forestMap2

-}

import Lazy.LList as LL exposing (LList)


{-| -}
type Tree a
    = Tree a (Forest a)


{-| -}
type alias Forest a =
    LList (Tree a)



-- Tree


{-| puts value in minimal tree context

    singleton "foo"
        |> item
    --> "foo"

    singleton "foo"
        |> isEmpty
    --> True

-}
singleton : a -> Tree a
singleton a =
    Tree a LL.empty


{-| Tree constructor

    tree "foo" LL.empty
        |> item
    --> "foo"


    fromList (\m _ -> m == Nothing) [ "bar", "baz" ]
        |> tree "foo"
        |> children
    --> [ "bar", "baz" ]

-}
tree : a -> Forest a -> Tree a
tree =
    Tree


{-| Check if tree doesn't have any child.

    singleton "foo"
        |> isEmpty
    --> True

    singleton "foo"
        |> insert (singleton "bar")
        |> isEmpty
    --> False

-}
isEmpty : Tree a -> Bool
isEmpty =
    List.isEmpty << children


{-| Obtain item from tree

    singleton "foo"
        |> item
        |> "foo"

-}
item : Tree a -> a
item (Tree i _) =
    i


{-| Obtain children items

    singleton "foo"
        |> insert (singleton "bar")
        |> insert (singleton "baz")
        |> children
    --> [ "bar", "baz" ]

-}
children : Tree a -> List a
children =
    List.map item << LL.toList << descendants


{-| Obtain descendants as Forest from the tree

    import Lazy.LList as LL

    singleton "foo"
        |> insert (singleton "bar")
        |> insert (singleton "baz")
        |> descendants
        |> LL.map item
        |> LL.toList
    --> [ "bar", "baz" ]

    singleton "foo"
        |> insert (singleton "bar" |> insert (singleton "baz"))
        |> descendants
        |> LL.map (children)
        |> LL.toList
    --> [ [ "baz" ] ]

-}
descendants : Tree a -> Forest a
descendants (Tree _ d) =
    d


{-| Map function over tree

    singleton 1
        |> map ((+) 1)
        |> item
    --> 2

    singleton 1
        |> insert (singleton 2)
        |> insert (singleton 3)
        |> map ((*) 2)
        |> children
    --> [ 4, 6 ]

TODO: reevaluate this
** Be careful when comparing mapped Trees using `(==)`.**
Due to use of `LazyList` and lack of type classes in Elm `(==)` isn't reliable
for comparing Trees.

-}
map : (a -> b) -> Tree a -> Tree b
map predicate (Tree a forest) =
    tree (predicate a) <| forestMap predicate forest


{-| Map function over two trees

    import Lazy.LList as LL

    map2 (+) (singleton 1) (singleton 5)
        |> item
    --> 6

    tree 1 (LL.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> map2 (+) (tree 5 <| LL.fromList [ singleton 6, singleton 7 ])
        |> children
    --> [ 8, 10 ]

-}
map2 : (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2 predicate (Tree a1 f1) (Tree a2 f2) =
    tree (predicate a1 a2) <| forestMap2 predicate f1 f2


{-| Chain map operations

    import Lazy.LList as LL

    tree (,) (LL.fromList [ singleton (,), singleton (,), singleton (,) ])
        |> andMap (tree 1 <| LL.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> andMap (tree 5 <| LL.fromList [ singleton 6, singleton 7 ])
        |> children
    --> [ (2, 6), (3, 7) ]

-}
andMap : Tree a -> Tree (a -> b) -> Tree b
andMap =
    map2 (|>)


{-| Flatten Tree of Trees

    import Lazy.LList as LL

    singleton (singleton 1)
        |> flatten
        |> item
    --> 1

    tree (tree "foo" <| LL.fromList [ singleton "bar"]) (LL.fromList [ singleton <| singleton "baz" ])
        |> flatten
        |> children
    --> [ "bar", "baz" ]

-}
flatten : Tree (Tree a) -> Tree a
flatten (Tree (Tree item c) children) =
    tree item <| LL.append c <| LL.map flatten children


{-| Maping tree construction over Tree.

    import Lazy.LList as LL

    tree "foo" (LL.fromList [ singleton "bar", singleton "baz" ])
        |> andThen (\a -> tree a <| LL.fromList [ singleton <| a ++ " fighter" ])
        |> children
    --> [ "foo fighter", "bar", "baz" ]

-}
andThen : (a -> Tree b) -> Tree a -> Tree b
andThen fc =
    flatten << map fc


{-| Insert tree as children tree

    import Lazy.LList as LL

    singleton 1
        |> insert (singleton 2)
        |> insert (singleton 3)
        |> children
    --> [ 2, 3 ]

    singleton 1
        |> insert (singleton 2)
        |> item
    --> 1

-}
insert : Tree a -> Tree a -> Tree a
insert t (Tree item c) =
    tree item <| LL.append c <| LL.fromList [ t ]



-- Forest


{-| Construct tree from list

    import Lazy.LList as LL

    [ { id = 1, parent = Nothing }
    , { id = 2, parent = Nothing }
    , { id = 3, parent = Just 1 }
    ]
        |> fromList (\p i -> Maybe.map .id p == i.parent)
        |> LL.map (.id << item)
        |> LL.toList
    --> [ 1, 2 ]

    [ { id = 1, parent = Nothing }
    , { id = 2, parent = Nothing }
    , { id = 3, parent = Just 1 }
    , { id = 4, parent = Just 1 }
    , { id = 5, parent = Just 2 }
    ]
        |> fromList (\p i -> Maybe.map .id p == i.parent)
        |> LL.andThen descendants
        |> LL.map (.id << item)
        |> LL.toList
    --> [ 3, 4, 5 ]

-}
fromList : (Maybe a -> a -> Bool) -> List a -> Forest a
fromList isParent =
    fromList_ Nothing isParent


{-| Map function over forest

    import Lazy.LList as LL

    [ 1, 2, 3 ]
        |> fromList (\m _ -> m == Nothing)
        |> forestMap ((+) 1)
        |> LL.map item
        |> LL.toList
    --> [ 2, 3, 4 ]

-}
forestMap : (a -> b) -> Forest a -> Forest b
forestMap predicate =
    LL.map (map predicate)


{-| Map function over two forests

    import Lazy.LList as LL

    [ 1, 2, 3 ]
        |> fromList (\m _ -> m == Nothing)
        |> forestMap2 (+) (fromList (\m _ -> m == Nothing) [1, 2])
        |> LL.map item
        |> LL.toList
    --> [ 2, 4 ]

-}
forestMap2 : (a -> b -> c) -> Forest a -> Forest b -> Forest c
forestMap2 predicate =
    LL.map2 (map2 predicate)



-- Private


constructTree : (Maybe a -> a -> Bool) -> List a -> a -> Tree a
constructTree isParent list item =
    tree item <| fromList_ (Just item) isParent list


fromList_ : Maybe a -> (Maybe a -> a -> Bool) -> List a -> Forest a
fromList_ parent isParent list =
    LL.llist (List.map (constructTree isParent list) << List.filter (isParent parent)) list
