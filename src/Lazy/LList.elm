module Lazy.LList
    exposing
        ( LList
        , andThen
        , append
        , cons
        , empty
        , filter
        , flatten
        , foldl
        , foldr
        , fromList
        , isEmpty
        , llist
        , map
        , map2
        , singleton
        , toList
        )

{-| This module implement lazy construction of strict List.
It is not Lazy List implementation and it has different charaktericstics.


# Types

@docs LList, empty, singleton, llist, isEmpty, fromList


# Operations

@docs cons, append


# Query

@docs isEmpty, toList


# Transformations

@docs map, map2, filter, foldr, foldl, flatten, andThen

-}

import Lazy exposing (Lazy)


{-| -}
type alias LList a =
    Lazy (List a)


{-| Init empty LList

    toList empty
    --> []

-}
empty : LList a
empty =
    Lazy.lazy <| \() -> []


{-| Init singleton LList

    singleton "foo"
        |> toList
    --> [ "foo" ]

-}
singleton : a -> LList a
singleton =
    llist List.singleton


{-| Init LList using constructor

    llist (List.range 0) 5
        |> toList
    --> [ 0, 1, 2, 3, 4, 5 ]

    llist (List.filter <| \a -> a % 2 == 0) (List.range 0 10)
        |> toList
    --> [ 0, 2, 4, 6, 8, 10 ]

-}
llist : (a -> List b) -> a -> LList b
llist constructor arg =
    Lazy.lazy <| \() -> constructor arg


{-| Check if LList is empty

    isEmpty empty
    --> True

    isEmpty (singleton "foo")
    --> False

    llist (List.filter <| \a -> a > 10) [ 1, 2, 3 ]
        |> isEmpty
    --> True

-}
isEmpty : LList a -> Bool
isEmpty =
    List.isEmpty << Lazy.force


{-| Map function over LList

    llist (List.range 0) 5
        |> map ((*) 2)
        |> toList
    --> [ 0, 2, 4, 6, 8, 10 ]

-}
map : (a -> b) -> LList a -> LList b
map predicate =
    Lazy.map (List.map predicate)


{-| Transform Eager evaluated List to LList

    fromList [ "foo", "bar", "baz" ]
        |> toList
    --> [ "foo", "bar", "baz" ]

-}
fromList : List a -> LList a
fromList =
    Lazy.lazy << always


{-| Map two functions over LList

    llist (List.range 0) 5
        |> map2 (+) (llist (List.range 0) 5)
        |> toList
    --> [ 0, 2, 4, 6, 8, 10 ]

-}
map2 : (a -> b -> c) -> LList a -> LList b -> LList c
map2 constructor =
    Lazy.map2 (List.map2 constructor)


{-| Append LList to LList

    append (singleton "foo") (fromList [ "bar", "baz" ])
        |> toList
    --> [ "foo", "bar", "baz" ]

-}
append : LList a -> LList a -> LList a
append =
    Lazy.map2 (++)


{-| Build List from LList

    toList empty
    --> []

    toList <| llist (List.range 0) 2
    --> [ 0, 1, 2 ]

-}
toList : LList a -> List a
toList =
    Lazy.force


{-| Add element to LList

    empty
        |> cons "bar"
        |> cons "foo"
        |> toList
    --> [ "foo", "bar" ]

-}
cons : a -> LList a -> LList a
cons a =
    Lazy.map ((::) a)


{-| Filter LList

    (cons 1 <| cons 2 <| cons 3 empty)
        |> filter ((<) 1)
        |> toList
    --> [ 2, 3 ]

-}
filter : (a -> Bool) -> LList a -> LList a
filter predicate =
    Lazy.map (List.filter predicate)


{-| Same as List.foldr but for LLists

    llist (List.range 0) 5
        |> foldr (+) 0
    --> 15

    llist (List.range 0) 3
        |> foldr (::) []
    --> [ 0, 1, 2, 3 ]

-}
foldr : (a -> b -> b) -> b -> LList a -> b
foldr predicate acc =
    List.foldr predicate acc << toList


{-| Same as List.foldl but for LLists

    llist (List.range 0) 5
        |> foldl (+) 0
    --> 15

    llist (List.range 0) 3
        |> foldl (::) []
    --> [ 3, 2, 1, 0 ]

-}
foldl : (a -> b -> b) -> b -> LList a -> b
foldl predicate acc =
    List.foldl predicate acc << toList


{-| Flatten LList

    fromList [ singleton "foo", cons "bar" <| singleton "baz" ]
       |> flatten
       |> toList
    --> [ "foo", "bar", "baz" ]

-}
flatten : LList (LList a) -> LList a
flatten =
    foldr append empty


{-| Map LList construction over LList

    cons "foo" (cons "bar" <| singleton "baz")
        |> andThen (\a -> cons a <| singleton (a ++ " fighter" ))
        |> toList
    --> [ "foo", "foo fighter", "bar", "bar fighter", "baz", "baz fighter" ]

-}
andThen : (a -> LList b) -> LList a -> LList b
andThen predicate =
    flatten << map predicate
