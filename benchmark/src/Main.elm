module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram)
import Lazy.Tree as Tree exposing (Tree)
import Lazy.Tree.Force as Tree


main : BenchmarkProgram
main =
    Benchmark.Runner.program <|
        describe "fromList vs fromKeyedList"
            [ createOnly
            , createAndExpand
            ]


createAndExpand : Benchmark
createAndExpand =
    describe "Create and expand everything"
        [ benchmark "fromList (10 elems)" <|
            \_ ->
                noodle10
                    |> Tree.fromList (\p i -> Maybe.map .id p == i.parent)
                    |> Tree.forceForest
        , benchmark "fromKeyedList (10 elems)" <|
            \_ ->
                noodle10
                    |> Tree.fromKeyedList .id .parent
                    |> Tree.forceForest
        , benchmark "fromList (100 elems)" <|
            \_ ->
                noodle100
                    |> Tree.fromList (\p i -> Maybe.map .id p == i.parent)
                    |> Tree.forceForest
        , benchmark "fromKeyedList (100 elems)" <|
            \_ ->
                noodle100
                    |> Tree.fromKeyedList .id .parent
                    |> Tree.forceForest
        ]


createOnly : Benchmark
createOnly =
    describe "Create only"
        [ benchmark "fromList (10)" <|
            \_ ->
                noodle10
                    |> Tree.fromList (\p i -> Maybe.map .id p == i.parent)
        , benchmark "fromKeyedList (10)" <|
            \_ ->
                noodle10
                    |> Tree.fromKeyedList .id .parent
        , benchmark "fromList (100)" <|
            \_ ->
                noodle100
                    |> Tree.fromList (\p i -> Maybe.map .id p == i.parent)
        , benchmark "fromKeyedList (100)" <|
            \_ ->
                noodle100
                    |> Tree.fromKeyedList .id .parent
        ]


type alias Item =
    { id : Int
    , parent : Maybe Int
    }


{-| Tree that looks like "1 -> 2 -> 3 -> ... -> n"
-}
noodle : Int -> List Item
noodle n =
    List.range 1 n
        |> List.map
            (\i ->
                { parent = Just i
                , id = i + 1
                }
            )
        |> (::)
            { parent = Nothing
            , id = 1
            }


noodle10 : List Item
noodle10 =
    noodle 10


noodle100 : List Item
noodle100 =
    noodle 100
