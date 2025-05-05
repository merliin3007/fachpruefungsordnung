module Persistent.Tree (Tree (..), Node (..), Hashed (..), hashTree) where

import Data.Text as T
import GHC.Int
import Persistent.Utility

data Tree a = Tree a [Tree a]

data Node = Node
  { objectId :: Int32,
    kind :: Text,
    content :: Text
  }
  deriving (Show)

data Hashed a = Hashed
  { hash :: Text,
    value :: a
  }

hashTree :: Tree Node -> Tree (Hashed Node)
hashTree (Tree object children) = Tree (Hashed treeHash object) hashedChildren
  where
    treeHash = hashText $ T.concat $ pack (show $ objectId object) : kind object : content object : childHashes
    childHashes = (\(Tree x _) -> hash x) <$> hashedChildren
    hashedChildren = hashTree <$> children
