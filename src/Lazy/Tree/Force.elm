module Lazy.Tree.Force exposing (..)

import Lazy
import Lazy.LList as LL
import Lazy.Tree exposing (Forest, Tree(..))


forceTree : Tree a -> Tree a
forceTree (Tree x xs) =
    Tree x (forceForest xs)


forceForest : Forest a -> Forest a
forceForest =
    Lazy.evaluate << LL.map forceTree
