{-# LANGUAGE DeriveGeneric #-}

module DTOs.Tree (Node (..), Edge (..), fromHashedTree) where

import Data.Text
import GHC.Generics
import Versioning.Hash
import qualified Versioning.Tree as T

data Node
  = Node
      { ref :: T.NodeID,
        kind :: Text,
        content :: Maybe Text,
        children :: [Edge]
      }
  | NodeRef
      {hash :: Hash}
  deriving (Show, Generic)

data Edge = Edge
  { title :: Text,
    node :: Node
  }
  deriving (Show, Generic)

fromHashedTree :: T.TreeRef (Hashed T.NodeWithRef) -> Node
fromHashedTree (T.Ref h) = NodeRef h
fromHashedTree
  ( T.Value
      ( T.Tree
          (Hashed _ (T.NodeWithRef ref' (T.Node kind' content')))
          children'
        )
    ) =
    Node ref' kind' content' $ mapChild <$> children'
    where
      mapChild :: T.Edge (Hashed T.NodeWithRef) -> Edge
      mapChild (T.Edge label node') = Edge label $ fromHashedTree node'
