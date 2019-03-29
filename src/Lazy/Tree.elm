module Lazy.Tree exposing
    ( Tree(..), Forest, singleton, build, fromList
    , isEmpty, item, children, descendants
    , insert
    , map, map2, filter, filterMap, sort, sortBy, sortWith, andMap, flatten, andThen
    , forestMap, forestMap2
    )

{-| This module implements Rose Tree data structure.

> In computing, a multi-way tree or rose tree is a tree data structure
> with a variable and unbounded number of branches per node.

This particular implementation uses lazy list construction (using `LList` module)
to lazily evaluate levels of Tree.


# Types & Constructor

@docs Tree, Forest, singleton, build, fromList


# Query

@docs isEmpty, item, children, descendants


# Modify

@docs insert


# Transforms

@docs map, map2, filter, filterMap, sort, sortBy, sortWith, andMap, flatten, andThen


# Forest

@docs forestMap, forestMap2

-}

import Lazy.LList as LL exposing (LList)


{-| \*\* Be careful when comparing `Tree`s using `(==)`.\*\*
Due to use of laziness `(==)` isn't reliable for comparing Trees.
-}
type Tree a
    = Tree a (Forest a)


{-| \*\* Be careful when comparing `Forest`s using `(==)`.\*\*
Due to use of laziness `(==)` isn't reliable for comparing Forests.
-}
type alias Forest a =
    LList (Tree a)



-- Tree


{-| Puts value in minimal `Tree` context

    singleton "foo"
        |> item
    --> "foo"

-}
singleton : a -> Tree a
singleton a =
    Tree a LL.empty


{-| Build `Tree` using custom constructor.

This can be for instance used to build `Tree` from other recursive data structure:

    type Item = Item String (List Item)

    getChildren (Item _ children) = children

    Item "foo" [ Item "bar" [], Item "baz" []]
        |> build getChildren
        |> children
    -> [ Item "bar" [], Item "baz" [] ]

Or lookups to some other data structure.

    import Dict exposing (Dict)

    rootItem : String
    rootItem = "foo"

    childrenDict : Dict String (List String)
    childrenDict = Dict.fromList [ ("foo", [ "bar", "baz" ]) ]

    build (\i -> Maybe.withDefault [] <| Dict.get i childrenDict) rootItem
        |> children
    --> [ "bar", "baz" ]

-}
build : (a -> List a) -> a -> Tree a
build getChildren root =
    Tree root <| LL.map (build getChildren) <| LL.llist getChildren root


{-| Check if `Tree` doesn't have any child.

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


{-| Obtain item from `Tree`.

    singleton "foo"
        |> item
        |> "foo"

-}
item : Tree a -> a
item (Tree i _) =
    i


{-| Obtain children items of `Tree`.

    singleton "foo"
        |> insert (singleton "bar")
        |> insert (singleton "baz")
        |> children
    --> [ "bar", "baz" ]

-}
children : Tree a -> List a
children =
    List.map item << LL.toList << descendants


{-| Obtain descendants as `Forest` from the `Tree`.

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


{-| Map function over `Tree`.

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

-}
map : (a -> b) -> Tree a -> Tree b
map fc (Tree a forest) =
    Tree (fc a) <| forestMap fc forest


{-| Map function over two `Tree`s

    map2 (+) (singleton 1) (singleton 5)
        |> item
    --> 6

    import Lazy.LList as LL

    Tree 1 (LL.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> map2 (+) (Tree 5 <| LL.fromList [ singleton 6, singleton 7 ])
        |> children
    --> [ 8, 10 ]

-}
map2 : (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2 fc (Tree a1 f1) (Tree a2 f2) =
    Tree (fc a1 a2) <| forestMap2 fc f1 f2


{-| Filter `Tree` children by given function.

This function goes from children of root downwards.
This means that nodes that don't satisfy predicate
are excluded and filter is never performed over their children
even if on those it might pass.

    import Lazy.LList as LL

    Tree 1 (LL.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> filter ((>) 4)
        |> children
    --> [ 2, 3 ]

    Tree 1 (LL.fromList [ insert (singleton 5) <| singleton 2, insert (singleton 6) <| singleton 3, singleton 4 ])
        |> filter ((<) 2)
        |> descendants
        |> LL.map children
        |> LL.toList
    --> [ [ 6 ], [] ]

-}
filter : (a -> Bool) -> Tree a -> Tree a
filter predicate (Tree treeItem c) =
    Tree treeItem <| LL.filterMap (filter_ predicate) c


filter_ : (a -> Bool) -> Tree a -> Maybe (Tree a)
filter_ predicate (Tree treeItem c) =
    if predicate treeItem then
        Just <| Tree treeItem <| LL.filterMap (filter_ predicate) c

    else
        Nothing


{-| FilterMap on `Tree`. Works similarly to `List.filterMap` with the same properties as [filter](#filter).
In case of `filterMap` even root node has to satisfy predicate otherwise
`Nothing` is returned.

    import Lazy.LList as LL

    Tree 1 (LL.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> filterMap (\a -> if a < 4 then Just (a * 2) else Nothing)
        |> Maybe.map children
    --> Just [ 4, 6 ]

    Tree 1 (LL.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> filterMap (\a -> if a > 2 then Just (a * 2) else Nothing)
        |> Maybe.map children
    --> Nothing

-}
filterMap : (a -> Maybe b) -> Tree a -> Maybe (Tree b)
filterMap predicate (Tree treeItem c) =
    predicate treeItem
        |> Maybe.map (\i -> Tree i <| LL.filterMap (filterMap predicate) c)


{-| Sort `Tree`.

    singleton 10
        |> insert (singleton 5)
        |> insert (singleton 2)
        |> sort
        |> children
    --> [ 2, 5 ]

it applies to all levels:

    import Lazy.LList as LL

    singleton 10
        |> insert (Tree 20 <| LL.llist (List.reverse << List.map singleton << List.range 1) 5)
        |> sort
        |> descendants
        |> LL.map children
        |> LL.toList
    --> [ [ 1, 2, 3, 4, 5 ] ]

-}
sort : Tree comparable -> Tree comparable
sort (Tree a f) =
    Tree a <| LL.map sort <| LL.sortBy item f


{-| Sort `Tree` by a function.

    singleton { val = 10 }
       |> insert (singleton { val = 7 })
       |> insert (singleton { val = 3 })
       |> sortBy .val
       |> children
    --> [ { val = 3 }, { val = 7 } ]

it applies to all levels:

    import Lazy.LList as LL

    singleton { a = 10 }
        |> insert (Tree { a = 20 } <| LL.llist (List.reverse << List.map (\v -> singleton { a = v }) << List.range 1) 3)
        |> sortBy .a
        |> descendants
        |> LL.map children
        |> LL.toList
    --> [ [ { a = 1 }, { a = 2 }, { a = 3 } ] ]

-}
sortBy : (a -> comparable) -> Tree a -> Tree a
sortBy predicate (Tree a f) =
    Tree a <|
        LL.map (sortBy predicate) <|
            LL.sortBy (predicate << item) f


{-| Sort `Tree` using custom Ordering function

    flippedComparison : comparable -> comparable -> Order
    flippedComparison a b =
        case Basics.compare a b of
            LT -> GT
            EQ -> EQ
            GT -> LT

    singleton 10
        |> insert (singleton 2)
        |> insert (singleton 5)
        |> sortWith flippedComparison
        |> children
    --> [ 5, 2 ]

-}
sortWith : (a -> a -> Order) -> Tree a -> Tree a
sortWith predicate (Tree a f) =
    Tree a <|
        LL.map (sortWith predicate) <|
            LL.sortWith (\fst snd -> predicate (item fst) (item snd)) f


{-| Chain map operations.

    import Lazy.LList as LL
    import Tuple

    Tree Tuple.pair (LL.fromList [ singleton Tuple.pair, singleton Tuple.pair, singleton Tuple.pair ])
        |> andMap (Tree 1 <| LL.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> andMap (Tree 5 <| LL.fromList [ singleton 6, singleton 7 ])
        |> children
    --> [ (2, 6), (3, 7) ]

-}
andMap : Tree a -> Tree (a -> b) -> Tree b
andMap =
    map2 (|>)


{-| Flatten `Tree` of Trees.

    singleton (singleton 1)
        |> flatten
        |> item
    --> 1

    import Lazy.LList as LL

    Tree (Tree "foo" <| LL.fromList [ singleton "bar"]) (LL.fromList [ singleton <| singleton "baz" ])
        |> flatten
        |> children
    --> [ "baz", "bar" ]

-}
flatten : Tree (Tree a) -> Tree a
flatten (Tree (Tree treeItem c) treeChildren) =
    Tree treeItem <| LL.append (LL.map flatten treeChildren) c


{-| Map given function onto a `Tree` and flatten the result.

    import Lazy.LList as LL

    singleton "foo"
        |> insert (singleton "bar")
        |> insert (singleton "baz")
        |> andThen (\a -> Tree a <| LL.fromList [ singleton <| a ++ " fighter" ])
        |> children
    --> [ "bar", "baz", "foo fighter" ]

-}
andThen : (a -> Tree b) -> Tree a -> Tree b
andThen fc =
    flatten << map fc


{-| Insert one `Tree` as children another.

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
insert t (Tree treeItem c) =
    Tree treeItem <| LL.append c <| LL.fromList [ t ]



-- Forest


{-| Construct `Forest` from a list.

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


fromList_ : Maybe a -> (Maybe a -> a -> Bool) -> List a -> Forest a
fromList_ parent isParent list =
    LL.llist (List.map (constructTree isParent list) << List.filter (isParent parent)) list


{-| Map function over `Forest`.

    import Lazy.LList as LL

    [ 1, 2, 3 ]
        |> fromList (\m _ -> m == Nothing)
        |> forestMap ((+) 1)
        |> LL.map item
        |> LL.toList
    --> [ 2, 3, 4 ]

-}
forestMap : (a -> b) -> Forest a -> Forest b
forestMap fc =
    LL.map (map fc)


{-| Map function over two `Forest`s.

    import Lazy.LList as LL

    [ 1, 2, 3 ]
        |> fromList (\m _ -> m == Nothing)
        |> forestMap2 (+) (fromList (\m _ -> m == Nothing) [1, 2])
        |> LL.map item
        |> LL.toList
    --> [ 2, 4 ]

-}
forestMap2 : (a -> b -> c) -> Forest a -> Forest b -> Forest c
forestMap2 fc =
    LL.map2 (map2 fc)



-- Private


constructTree : (Maybe a -> a -> Bool) -> List a -> a -> Tree a
constructTree isParent list treeItem =
    Tree treeItem <| fromList_ (Just treeItem) isParent list
