module Lazy.Tree
    exposing
        ( Forest
        , Tree
        , andMap
        , andThen
        , build
        , children
        , descendants
        , filter
        , filterMap
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


# Types & Constructors

@docs Tree, Forest, singleton, tree, build, fromList


# Query

@docs isEmpty, item, children, descendants


# Modify

@docs insert


# Transforms

@docs map, map2, filter, filterMap, andMap, flatten, andThen


# Forest

@docs forestMap, forestMap2

-}

import Lazy.LList as LL exposing (LList)


{-| ** Be careful when comparing Trees using `(==)`.**
Due to use of lazyness and lack of ad hoc polymorphism in Elm `(==)` isn't reliable
for comparing Trees.
-}
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


    fromList (\m l -> if m == Nothing then l else []) [ "bar", "baz" ]
        |> tree "foo"
        |> children
    --> [ "bar", "baz" ]

-}
tree : a -> Forest a -> Tree a
tree =
    Tree


{-| Build `Tree` using custom constructor.

This can be for instance used to build tree from other recursive data structre:

    type Item = Item String (List Item)

    getChildren (Item _ children) = children

    Item "foo" [ Item "bar" [], Item "baz" []]
        |> from getChildren
        |> children
    -> [ Item "bar" [], Item "baz" [] ]

Or you can use this function for any sort of custom lookups:

    import Dict exposing (Dict)

    rootItem : String
    rootItem = "foo"

    childrenDict : Dict String (List String)
    childrenDict = Dict.fromList [ ("foo", [ "bar", "baz" ]) ]

    from (Maybe.withDefault [] << flip Dict.get childrenDict) rootItem
        |> children
    --> [ "bar", "baz" ]

-}
build : (a -> List a) -> a -> Tree a
build getChildren root =
    tree root <| LL.map (from getChildren) <| LL.llist getChildren root


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

-}
map : (a -> b) -> Tree a -> Tree b
map predicate (Tree a forest) =
    tree (predicate a) <| forestMap predicate forest


{-| Map function over two trees

    map2 (+) (singleton 1) (singleton 5)
        |> item
    --> 6

    import Lazy.LList as LL

    tree 1 (LL.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> map2 (+) (tree 5 <| LL.fromList [ singleton 6, singleton 7 ])
        |> children
    --> [ 8, 10 ]

-}
map2 : (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2 predicate (Tree a1 f1) (Tree a2 f2) =
    tree (predicate a1 a2) <| forestMap2 predicate f1 f2


{-| Filter Tree children by given function

This function goes from children of root downwards.
This means that nodes that doesn't satisfy predicate
are excluded and filter is never performed over their children
even if on those it might pass.

    tree 1 (LL.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> filter ((>) 4)
        |> children
    --> [ 2, 3 ]

    tree 1 (LL.fromList [ insert (singleton 5) <| singleton 2, insert (singleton 6) <| singleton 3, singleton 4 ])
        |> filter ((<) 2)
        |> descendants
        |> LL.map children
        |> LL.toList
    --> [ [ 6 ], [] ]

-}
filter : (a -> Bool) -> Tree a -> Tree a
filter predicate (Tree item c) =
    tree item <| LL.filterMap (filter_ predicate) c


filter_ : (a -> Bool) -> Tree a -> Maybe (Tree a)
filter_ predicate (Tree item c) =
    if predicate item then
        Just <| tree item <| LL.filterMap (filter_ predicate) c
    else
        Nothing


{-| Filter map on Tree. Works similarly to [filter](#filter).
In case offilterMap even root node as to satisfy predicate otherwise
Nothing is returned.

    tree 1 (LL.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> filterMap (\a -> if a < 4 then Just (a * 2) else Nothing)
        |> Maybe.map children
    --> Just [ 4, 6 ]

    tree 1 (LL.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> filterMap (\a -> if a > 2 then Just (a * 2) else Nothing)
        |> Maybe.map children
    --> Nothing

-}
filterMap : (a -> Maybe b) -> Tree a -> Maybe (Tree b)
filterMap predicate (Tree item c) =
    predicate item
        |> Maybe.map (\i -> tree i <| LL.filterMap (filterMap predicate) c)


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

    singleton (singleton 1)
        |> flatten
        |> item
    --> 1

    import Lazy.LList as LL

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

    singleton "foo"
        |> insert (singleton "bar")
        |> insert (singleton "baz")
        |> andThen (\a -> tree a <| LL.fromList [ singleton <| a ++ " fighter" ])
        |> children
    --> [ "foo fighter", "bar", "baz" ]

-}
andThen : (a -> Tree b) -> Tree a -> Tree b
andThen fc =
    flatten << map fc


{-| Insert tree as children tree

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
        |> fromList (\p -> List.filter (\i -> Maybe.map .id p == i.parent))
        |> LL.map (.id << item)
        |> LL.toList
    --> [ 1, 2 ]

    [ { id = 1, parent = Nothing }
    , { id = 2, parent = Nothing }
    , { id = 3, parent = Just 1 }
    , { id = 4, parent = Just 1 }
    , { id = 5, parent = Just 2 }
    ]
        |> fromList (\p -> List.filter (\i -> Maybe.map .id p == i.parent))
        |> LL.andThen descendants
        |> LL.map (.id << item)
        |> LL.toList
    --> [ 3, 4, 5 ]

-}
fromList : (Maybe a -> List a -> List a) -> List a -> Forest a
fromList construct =
    fromList_ Nothing construct


constructTree : (Maybe a -> List a -> List a) -> List a -> a -> Tree a
constructTree construct list item =
    tree item <| fromList_ (Just item) construct list


fromList_ : Maybe a -> (Maybe a -> List a -> List a) -> List a -> Forest a
fromList_ parent construct list =
    LL.llist (List.map (constructTree construct list) << construct parent) list


{-| Map function over forest

    import Lazy.LList as LL

    [ 1, 2, 3 ]
        |> fromList (\m l -> if m == Nothing then l else [])
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
        |> fromList (\m l -> if m == Nothing then l else [])
        |> forestMap2 (+) (fromList (\m l -> if m == Nothing then l else []) [1, 2])
        |> LL.map item
        |> LL.toList
    --> [ 2, 4 ]

-}
forestMap2 : (a -> b -> c) -> Forest a -> Forest b -> Forest c
forestMap2 predicate =
    LL.map2 (map2 predicate)
