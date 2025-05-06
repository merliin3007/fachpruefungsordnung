module Language.Ltml.Node (Node) where

import Language.Ltml.Label (Label)

data Node a = Node (Maybe Label) a
