module Docs.Tree
    ( Tree (..)
    , Edge (..)
    ) where

import Data.Text (Text)

data Tree a
    = Node Text [Edge a]
    | Leaf a

data Edge a = Edge Text (Tree a)

instance Functor Tree where
    fmap f (Node header edge) = Node header $ (f <$>) <$> edge
    fmap f (Leaf x) = Leaf $ f x

instance Functor Edge where
    fmap f (Edge label tree) = Edge label $ f <$> tree
