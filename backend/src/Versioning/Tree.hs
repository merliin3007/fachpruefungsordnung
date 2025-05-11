{-# LANGUAGE DeriveGeneric #-}

module Versioning.Tree
  ( Tree (..),
    Ref (..),
    TreeRef,
    Edge (..),
    NodeWithMaybeRef (..),
    NodeWithRef (..),
    Node (..),
    DataEdge (..),
    treeRefHash,
    dataEdges,
    hashedTree,
    hashTree,
    mkTree,
    mkEdge,
  )
where

import qualified Crypto.Hash.SHA1 as SHA1
import Data.Aeson (ToJSON)
import Data.Function ((&))
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics
import GHC.Int
import Versioning.Hash (Hash (..), Hashable (..), Hashed (..))

type NodeId = Int32

-- either a reference to an object in the database or the object itself
data Ref a b
  = Ref a
  | Value b
  deriving (Show, Generic)

instance (ToJSON a, ToJSON b) => ToJSON (Ref a b)

instance (ToSchema a, ToSchema b) => ToSchema (Ref a b)

-- a tree (TODO: rename Object)
data Tree a = Tree a [Edge a] deriving (Show, Generic)

instance (ToJSON a) => ToJSON (Tree a)

-- an edge of a tree
data Edge a = Edge Text (TreeRef a) deriving (Show, Generic)

instance (ToJSON a) => ToJSON (Edge a)

-- a reference to a tree
type TreeRef a = Ref Hash (Tree a)

treeRefHash :: (Hashable a) => TreeRef (Hashed a) -> Hash
treeRefHash (Ref ref) = ref
treeRefHash (Value (Tree (Hashed ref _) _)) = ref

-- convenience constructor for an edge
mkEdge :: Text -> Tree NodeWithMaybeRef -> Edge NodeWithMaybeRef
mkEdge label tree = Edge label $ Value tree

-- convenience constructor for a tree
mkTree :: Node -> [Edge NodeWithMaybeRef] -> Tree NodeWithMaybeRef
mkTree node = Tree (NodeWithMaybeRef Nothing node)

-- construct a hashed tree from a node and its already hashed edges
hashedTree ::
  NodeWithRef ->
  [Edge (Hashed NodeWithRef)] ->
  Tree (Hashed NodeWithRef)
hashedTree self children = Tree (Hashed (Hash (SHA1.finalize (foldr hashEdge (updateHash SHA1.init self) children))) self) children
  where
    hashEdge (Edge label childEdge) ctx =
      updateHash ctx label
        & flip updateHash (treeRefHash childEdge)

-- construct a hashed tree
hashTree :: Tree NodeWithRef -> Tree (Hashed NodeWithRef)
hashTree (Tree self children) = hashedTree self $ hashEdge <$> children
  where
    hashEdge (Edge label treeRef) = Edge label $ case treeRef of
      Value tree -> Value (hashTree tree)
      Ref ref -> Ref ref

-- a node version for a node which might not yet exist
data NodeWithMaybeRef
  = NodeWithMaybeRef (Maybe NodeId) Node
  deriving (Show, Generic)

instance ToJSON NodeWithMaybeRef

-- a node version for a node which is guaranteed to already exist
data NodeWithRef
  = NodeWithRef NodeId Node
  deriving (Show, Generic)

instance ToJSON NodeWithRef

instance Hashable NodeWithRef where
  updateHash ctx (NodeWithRef ref node) =
    updateHash ctx ref
      & flip updateHash (nodeKind node)
      & flip updateHash (nodeContent node)

-- a node version
data Node = Node
  { nodeKind :: Text,
    nodeContent :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON Node

-- a relational-style edge
data DataEdge = DataEdge
  { edgeParent :: Hash,
    edgeChild :: Hash,
    edgeChildPosition :: Int32,
    edgeChildTitle :: Text
  }

dataEdges :: Tree (Hashed NodeWithRef) -> [DataEdge]
dataEdges (Tree (Hashed parentHash _) children) =
  zipWith fromEdge [1, 2 ..] children
  where
    fromEdge position (Edge label child) =
      DataEdge
        { edgeParent = parentHash,
          edgeChild = treeRefHash child,
          edgeChildPosition = position,
          edgeChildTitle = label
        }
