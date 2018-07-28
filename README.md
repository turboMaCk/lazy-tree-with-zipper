# Tree with Zipper

[![Build Status](https://travis-ci.org/turboMaCk/lazy-tree-with-zipper.svg?branch=master)](https://travis-ci.org/turboMaCk/lazy-tree-with-zipper)

This is pure Elm [rose tree](https://en.wikipedia.org/wiki/Rose_tree)
with [zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)) implementation.
In context of Elm, this data structure is mostly useful for building hierarchical interfaces
like menus, data browsers or filters.

Main features of this library are things like easy building tree structure from flat Lists
with very good performance characteristics, powerful and extensible zipper and feature-rich API.

# Performance

`Tree` is using custom List like implementation (`LList`) to enable lazy level after level evaluation
of tree. In fact `LList` is just a function that construct plain old `List`. This id approach is the main performance optimization used in this library.

There is another library implementing same idea in slightly different way [tomjkidd/elm-multiway-tree-zipper](tomjkidd/elm-multiway-tree-zipper).
The main difference is that `elm-multiway-tree-zipper` implementation is strict so whole Tree is immediately evaluated.
Implementation provided by this package is optimalized for situations in which it isn't necessary to construct whole
structure immediately. In situations where Tree is expanded level by level this implementation yields
much better performance than strict implementation especially for large trees.
You can find basic comparison in [performance](https://github.com/turboMaCk/lazy-tree-with-zipper/blob/master/performance).

__This package is highly experimental and might change a lot over time.__

Feedback and contributions to both code and documentation are very welcome.

# Usage

As a pure Elm package preferable way to install is using elm package:

```
$ elm package install turboMaCk/lazy-tree-with-zipper
```

This is example of whole application that renders levels of items as nested tree
with toggling between open and closed state in every level:

```elm
module Main exposing (..)

import Html exposing (Html)
import Html.Events as Events
import Lazy.Tree as Tree exposing (Tree(Tree))
import Lazy.Tree.Zipper as Zipper exposing (Zipper)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }



-- Model


type alias Item =
    { id : Int
    , name : String
    , parent : Maybe Int
    }


items : List Item
items =
    [ { id = 1, name = "Foo", parent = Nothing }
    , { id = 2, name = "Bar", parent = Nothing }
    , { id = 3, name = "Baz", parent = Nothing }
    , { id = 4, name = "Fobar", parent = Just 1 }
    , { id = 5, name = "Bar child", parent = Just 2 }
    , { id = 6, name = "Foobar child", parent = Just 4 }
    ]


{-| Zipper of pair where first value means `isOpen` and second contain Item details.
-}
type alias Model =
    Zipper ( Bool, Item )


init : Model
init =
    let
        root =
            { id = -1, name = "root", parent = Nothing }
    in
    List.map ((,) False) items
        |> Tree.fromList (\p ( _, i ) -> Maybe.map (.id << Tuple.second) p == i.parent)
        |> Tree ( False, root )
        |> Zipper.fromTree



-- Update


type Msg
    = Toggle (Zipper ( Bool, Item ))


update : Msg -> Model -> Model
update (Toggle zipper) model =
    Zipper.updateItem (\( s, i ) -> ( not s, i )) zipper



-- View


view : Model -> Html Msg
view zipper =
    Html.ul [] [ viewLevel (Zipper.root zipper) ]


viewLevel : Zipper ( Bool, Item ) -> Html Msg
viewLevel zipper =
    let
        ( isOpen, item ) =
            Zipper.current zipper
    in
    Html.li []
        [ Html.a [ Events.onClick <| Toggle zipper ]
            [ if not (Zipper.isEmpty zipper) then
                Html.span []
                    [ if isOpen then
                        Html.text "- "
                      else
                        Html.text "+ "
                    ]
              else
                Html.text ""
            , Html.text item.name
            ]
        , Html.ul [] <|
            if isOpen then
                Zipper.openAll zipper
                    |> List.map viewLevel
            else
                []
        ]
```

# Background

I've spent about a year experimenting with different ideas of Rose Tree implementation
optimized for needs of building UIs for recursive data. The biggest turned out to be performance.
Usually, data for web applications are coming from a server which uses SQL database as storage.
API usually then renders flat JSON or any other data format which uses references to describe recursive relationships.
Therefore one of the main features that are needed is an efficient and easy way to build tree from a list of data.
This usually results in exponential complexity. Since one item might be a child of multiple other things
there has to be at least one iteration over the whole list of data. Also by definition using such data
for building rose tree might result in infinity deep resulting tree.

Those are the things I've experimented with over time:

- Strict based (`Tree a (List a)`) - not that great performance but OK until you hit too much recursion.
- Lazy List based implementation (`Tree a (LazyList a)`) - runs into too much recursion even on simpler data.
- Continuation Passing / CPS - very slow (hitting scripts runs for too long) - might have been an issue with a particular algorithm.
- Lazy List construction - this is what this package is using - very best performance.

# License

This package is released under BSD-3-Clause license. See [LICENSE](LICENSE) file for more info.
