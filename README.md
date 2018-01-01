# Tree with Zipper

This is pure [Elm](http://elm-lang.org/) [rose tree](https://en.wikipedia.org/wiki/Rose_tree)
with [zipper](https://en.wikipedia.org/wiki/Rose_tree) implementation.
In context of Elm, this data structure is mostly useful for building hierarchical interfaces
like menus, folder browsers or filters.

Main features of this library are things like easily building tree structure from flat list
with very good performance characteristics, powerful and extensible zipper and feature-rich API.

__This package is highly experimental and might change a lot over time.__

Feedback and contributions to both code and documentation are very welcome.

# Usage

As a pure Elm package preferable way to install is using elm package:

```
$ elm package install turboMaCk/tree-with-zipper
```

This is example of whole application that renders levels of items as nested tree
with toggling between open and closed state in every level:

```elm
module Main exposing (..)

import Html exposing (Html)
import Html.Events as Events
import Lazy.Tree as Tree
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
        |> Tree.tree ( False, root )
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

# Performance

`Tree` is using custom List like implementation (`LList`) to enable lazy level after level evaluation
of tree. This approach is the main approach to performance optimization this package takes.

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
- Lazy List construction - implemented in two ways (this package ins 2nd implementation of the idea) - very best performance.

# License

This package is released under BSD-3-Clause license. See [LICENSE](LICENSE) file for more info.
