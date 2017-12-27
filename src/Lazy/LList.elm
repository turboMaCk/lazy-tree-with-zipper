module Lazy.LList exposing (..)

{-| This module implement lazy construction of strict List.
It is not Lazy List implementation and it has different charaktericstics.
-}

import Lazy exposing (Lazy)


type alias LList a =
    Lazy (List a)


empty : LList a
empty =
    Lazy.lazy <| \() -> []


llist : (a -> List b) -> a -> LList b
llist constructor arg =
    Lazy.lazy <| \() -> constructor arg


isEmpty : LList a -> Bool
isEmpty =
    List.isEmpty << Lazy.force


map : (a -> b) -> LList a -> LList b
map predicate =
    Lazy.map (List.map predicate)


fromList : List a -> LList a
fromList =
    Lazy.lazy << always


map2 : (a -> b -> c) -> LList a -> LList b -> LList c
map2 constructor =
    Lazy.map2 (List.map2 constructor)


append : LList a -> LList a -> LList a
append =
    Lazy.map2 (++)


toList : LList a -> List a
toList =
    Lazy.force


cons : a -> LList a -> LList a
cons a =
    llist ((::) a << toList)


foldr : (a -> b -> b) -> b -> LList a -> b
foldr predicate acc =
    List.foldr predicate acc << toList


foldl : (a -> b -> b) -> b -> LList a -> b
foldl predicate acc =
    List.foldl predicate acc << toList


flatten : LList (LList a) -> LList a
flatten =
    foldr append empty


andThen : (a -> LList b) -> LList a -> LList b
andThen predicate =
    flatten << map predicate
