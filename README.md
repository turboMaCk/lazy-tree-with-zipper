# Tree with Zipper

This is pure [Elm](http://elm-lang.org/) [rose tree](https://en.wikipedia.org/wiki/Rose_tree)
with [zipper](https://en.wikipedia.org/wiki/Rose_tree) implementation.
In context of Elm this data structure is mostly useful for building hierarchical interfaces
like menus, folder browsers or filters.

Main features of this library are things like easily building tree structure from flat list
with very good performance charakteristics, powerful and extensible zipper and feature rich API.

__This package is highly experimental and might change a lot over time.__

Feedback and contributions to both code and documentation are very welcome.

# Usage

As a pure Elm package preferable way to install is using elm package:

```
$ elm package install turboMaCk/tree-with-zipper
```

```elm
```

# Performance

`Tree` is using custom List like implementation (`LList`) to enable lazy level after level evaluation
of tree. This approach is main approach to performance optimization this package takes.

# Background

I've spend about a year experimenting and trying different ideas of Rose Tree implementation
optimized for needs in building UIs for recursive data. The biggest turned out be performance.
Usually data for web applications are coming from server which uses SQL database as storage.
API usually then renders flat JSON or any other data format which defines recursive as relations ship.
Therefore one of the main feature that is needed is efficient and easy way to build tree from list of data.
This usually results in exponential complexity. Since one item might be child of multiple other things
there has to be at least one iteration over whole list of data. Also by definition using such data
for building rose tree might result in infinity deep resulting tree.

Those are the things I've experimented with over time:

- Strict based (`Tree a (List a)`) - not that great performance but OK until you hit too much recursion.
- Lazy List based implementation (`Tree a (LazyList a)`) - runs into too much recursion even on simpler data.
- Continuation Passing / CPS - very slow (hitting scripts runs for too long) - might have been issue with particular algorithm.
- Lazy List construction - implemented in two ways (this package ins 2nd implementation of idea) - very best performance.

# License

This package is released under BSD-3-Clause license. See [LICENSE](LICENSE) file for more info.
