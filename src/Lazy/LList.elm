module Lazy.LList exposing
    ( LList, empty, singleton, llist, fromList
    , cons, append
    , isEmpty, toList, head, tail
    , map, map2, filter, filterMap, reverse, sort, sortBy, sortWith, foldr, foldl, lazyFoldr, lazyFoldl, concat, andThen
    )

{-| This module implements lazy construction of strict List.
It is not Lazy List implementation and it has different characteristics.
It's mostly intended for internal purposes of this library and is exposed
just in case some additional user extensions will need it.


# Types & Constructors

@docs LList, empty, singleton, llist, fromList


# Operations

@docs cons, append


# Query

@docs isEmpty, toList, head, tail


# Transformations

@docs map, map2, filter, filterMap, reverse, sort, sortBy, sortWith, foldr, foldl, lazyFoldr, lazyFoldl, concat, andThen

-}

import Lazy exposing (Lazy)


{-| **`LList` uses lazy implementation and therefore can't be compared using `==`**
-}
type alias LList a =
    Lazy (List a)


{-| Init empty `LList`.

    toList empty
    --> []

-}
empty : LList a
empty =
    llist identity []


{-| Init singleton `LList`.

    singleton "foo"
        |> toList
    --> [ "foo" ]

-}
singleton : a -> LList a
singleton =
    llist List.singleton


{-| Init `LList` using constructor.

`LList` is init as a function from `a -> List b`.
Evaluation of this function is lazy and happens in time when
actuall value is needed, not when constructor is called.

For instance you can use some `List` constructor:

    llist (List.range 0) 5
        |> toList
    --> [ 0, 1, 2, 3, 4, 5 ]

Or use any other function you need`List`:

    llist (List.filter <| \a -> a % 2 == 0) (List.range 0 10)
        |> toList
    --> [ 0, 2, 4, 6, 8, 10 ]

-}
llist : (a -> List b) -> a -> LList b
llist constructor arg =
    Lazy.lazy <| \() -> constructor arg


{-| Construct `LList` from `List`.

    fromList [ "foo", "bar", "baz" ]
        |> toList
    --> [ "foo", "bar", "baz" ]

-}
fromList : List a -> LList a
fromList =
    llist identity


{-| Add element to `LList`.

    empty
        |> cons "bar"
        |> cons "foo"
        |> toList
    --> [ "foo", "bar" ]

This function is performed lazily.

-}
cons : a -> LList a -> LList a
cons a =
    Lazy.map ((::) a)


{-| Append `LList` to `LList`.

    append (singleton "foo") (fromList [ "bar", "baz" ])
        |> toList
    --> [ "foo", "bar", "baz" ]

This function is performed lazily.

-}
append : LList a -> LList a -> LList a
append =
    Lazy.map2 (++)


{-| Check if `LList` is empty.

    isEmpty empty
    --> True

    isEmpty (singleton "foo")
    --> False

    llist (List.filter <| \a -> a > 10) [ 1, 2, 3 ]
        |> isEmpty
    --> True

This function forces evaluation.

-}
isEmpty : LList a -> Bool
isEmpty =
    List.isEmpty << toList


{-| Build `List` from `LList`.

    toList empty
    --> []

    toList <| llist (List.range 0) 2
    --> [ 0, 1, 2 ]

This function forces evaluation.

-}
toList : LList a -> List a
toList =
    Lazy.force


{-| Get first element from `LList`.

    head empty
    --> Nothing

    llist (List.range 0) 2
       |> head
    --> Just 0

This function forces evaluation.

-}
head : LList a -> Maybe a
head =
    List.head << toList


{-| Get first element from `LList`.

    tail empty
    --> Nothing

    tail (singleton "foo")
        |> Maybe.map toList
    --> Just []

    llist (List.range 0) 2
        |> tail
        |> Maybe.map toList
    --> Just [ 1, 2 ]

This function forces evaluation.

-}
tail : LList a -> Maybe (LList a)
tail =
    Maybe.map fromList << List.tail << toList


{-| Map function over `LList`.

    llist (List.range 0) 5
        |> map ((*) 2)
        |> toList
    --> [ 0, 2, 4, 6, 8, 10 ]

This function is performed lazily.

-}
map : (a -> b) -> LList a -> LList b
map predicate =
    Lazy.map (List.map predicate)


{-| Map two functions over `LList`.

    llist (List.range 0) 5
        |> map2 (+) (llist (List.range 0) 5)
        |> toList
    --> [ 0, 2, 4, 6, 8, 10 ]

This function is performed lazily.

-}
map2 : (a -> b -> c) -> LList a -> LList b -> LList c
map2 constructor =
    Lazy.map2 (List.map2 constructor)


{-| Filter `LList`.

    (cons 1 <| cons 2 <| cons 3 empty)
        |> filter ((<) 1)
        |> toList
    --> [ 2, 3 ]

This function is performed lazily.

-}
filter : (a -> Bool) -> LList a -> LList a
filter predicate =
    Lazy.map (List.filter predicate)


{-| Similar to `List.filterMap` but for `LList`.

    (cons 1 <| cons 2 <| cons 3 empty)
        |> filterMap (\a -> if 1 < a then Just (2 * a) else Nothing)
        |> toList
    --> [ 4, 6 ]

This function is performed lazily.

-}
filterMap : (a -> Maybe b) -> LList a -> LList b
filterMap predicate =
    Lazy.map (List.filterMap predicate)


{-| Reverse `LList`.

    fromList [ 1, 2, 3 ]
        |> reverse
        |> toList
    --> [ 3, 2, 1 ]

This function is performed lazily.

-}
reverse : LList a -> LList a
reverse =
    Lazy.map List.reverse


{-| Sort by for `LList`.

    fromList [ 3, 1, 2 ]
        |> sort
        |> toList
    --> [ 1, 2, 3 ]

This function is performed lazily.

-}
sort : LList comparable -> LList comparable
sort =
    Lazy.map List.sort


{-| Sort by for `LList`.

    fromList [ 3, 1, 2 ]
        |> sortBy identity
        |> toList
    --> [ 1, 2, 3 ]

    fromList [ { val = "c"} , { val = "b"}, { val = "a"} ]
        |> sortBy .val
        |> toList
    -->  [ { val = "a"}, { val = "b"}, { val = "c"} ]

This function is performed lazily.

-}
sortBy : (a -> comparable) -> LList a -> LList a
sortBy predicate =
    Lazy.map (List.sortBy predicate)


{-| Sort with for `LList`

    flippedComparison : comparable -> comparable -> Order
    flippedComparison a b =
        case Basics.compare a b of
            LT -> GT
            EQ -> EQ
            GT -> LT

    llist (List.range 1) 5
        |> sortWith flippedComparison
        |> toList
    --> [ 5, 4, 3, 2, 1 ]

This function is performed lazily.

-}
sortWith : (a -> a -> Order) -> LList a -> LList a
sortWith predicate =
    Lazy.map (List.sortWith predicate)


{-| Same as `List.foldr` but for `LLists`.

    llist (List.range 0) 5
        |> foldr (+) 0
    --> 15

    llist (List.range 0) 3
        |> foldr (::) []
    --> [ 0, 1, 2, 3 ]

This function forces evaluation.

-}
foldr : (a -> b -> b) -> b -> LList a -> b
foldr predicate acc =
    List.foldr predicate acc << toList


{-| Same as `List.foldl` but for `LLists`.

    llist (List.range 0) 5
        |> foldl (+) 0
    --> 15

    llist (List.range 0) 3
        |> foldl (::) []
    --> [ 3, 2, 1, 0 ]

This function forces evaluation.

-}
foldl : (a -> b -> b) -> b -> LList a -> b
foldl predicate acc =
    List.foldl predicate acc << toList


{-| Lazy variant of `foldr`.

Works only with lazy values.

    import Lazy

    llist (List.range 0) 5
        |> lazyFoldr (\a -> Lazy.map ((+) a)) (Lazy.lazy <| always 0)
        |> Lazy.force
    --> 15

This function is performed lazily.

-}
lazyFoldr : (a -> Lazy b -> Lazy b) -> Lazy b -> LList a -> Lazy b
lazyFoldr predicate acc =
    Lazy.andThen (List.foldr predicate acc)


{-| Lazy variant of `foldl`.

Works only with lazy values.

    import Lazy

    llist (List.range 0) 5
        |> lazyFoldl (\a -> Lazy.map ((+) a)) (Lazy.lazy <| always 0)
        |> Lazy.force
    --> 15

This function is performed lazily.

-}
lazyFoldl : (a -> Lazy b -> Lazy b) -> Lazy b -> LList a -> Lazy b
lazyFoldl predicate acc =
    Lazy.andThen (List.foldl predicate acc)


{-| Flatten `LList`.

    fromList [ singleton "foo", cons "bar" <| singleton "baz" ]
       |> concat
       |> toList
    --> [ "foo", "bar", "baz" ]

This function is performed lazily.

-}
concat : LList (LList a) -> LList a
concat =
    lazyFoldr append empty


{-| Map `LList` construction over `LList`.

    cons "foo" (cons "bar" <| singleton "baz")
        |> andThen (\a -> cons a <| singleton (a ++ " fighter" ))
        |> toList
    --> [ "foo", "foo fighter", "bar", "bar fighter", "baz", "baz fighter" ]

This function is performed lazily.

-}
andThen : (a -> LList b) -> LList a -> LList b
andThen predicate =
    concat << map predicate
