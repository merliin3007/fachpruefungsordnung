{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Versioning.Tree
  ( Tree (..),
    Ref (..),
    TreeRef,
    Edge (..),
    NodeWithMaybeRef (..),
    NodeWithRef (..),
    Node (..),
    NodeID (..),
    DataEdge (..),
    editableTree,
    editableTreeRef,
    treeRefHash,
    dataEdges,
    hashedTree,
    hashTree,
    nodeIDInt32,
    mkTree,
    mkEdge,
  )
where

import qualified Crypto.Hash.SHA1 as SHA1
import           Data.Aeson       (ToJSON (..), (.=))
import qualified Data.Aeson       as Aeson
import           Data.Function    ((&))
import           Data.OpenApi     (ToSchema)
import           Data.Text        (Text)
import           GHC.Generics
import           GHC.Int
import           Versioning.Hash  (Hash (..), Hashable (..), Hashed (..))

newtype NodeID = NodeID Int32 deriving (Show, Generic)

instance ToJSON NodeID

instance Hashable NodeID where
  updateHash ctx (NodeID i) = updateHash ctx i

nodeIDInt32 :: NodeID -> Int32
nodeIDInt32 (NodeID i) = i

-- either a reference to an object in the database or the object itself
data Ref a b
  = Ref a
  | Value b
  deriving (Show, Generic)

instance Functor (Ref a) where
  fmap f (Value b) = Value $ f b
  fmap _ (Ref a)   = Ref a

instance (ToJSON a, ToJSON b) => ToJSON (Ref a b) where
  toJSON (Ref ref)     = Aeson.object ["ref" .= ref]
  toJSON (Value value) = toJSON value

instance (ToSchema a, ToSchema b) => ToSchema (Ref a b)

-- a tree (TODO: rename Object)
data Tree a = Tree a [Edge a] deriving (Show, Generic)

instance Functor Tree where
  fmap f (Tree a edges) = Tree (f a) $ fmap f <$> edges

instance (ToJSON a) => ToJSON (Tree a) where
  toJSON (Tree node edges) =
    Aeson.object ["node" .= node, "children" .= edges]

editableTree :: Tree (Hashed NodeWithRef) -> Tree NodeWithMaybeRef
editableTree = ((\(Hashed _ t) -> toNodeWithMaybeRef t) <$>)

editableTreeRef :: TreeRef (Hashed NodeWithRef) -> TreeRef NodeWithMaybeRef
editableTreeRef = (editableTree <$>)

-- an edge of a tree
data Edge a = Edge Text (TreeRef a) deriving (Show, Generic)

instance Functor Edge where
  fmap f (Edge label treeRef) = Edge label $ fmap f <$> treeRef

instance (ToJSON a) => ToJSON (Edge a) where
  toJSON (Edge label node) =
    Aeson.object ["title" .= label, "child" .= node]

-- a reference to a tree
type TreeRef a = Ref Hash (Tree a)

treeRefHash :: TreeRef (Hashed a) -> Hash
treeRefHash (Ref ref)                       = ref
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
      Ref ref    -> Ref ref

-- a node version for a node which might not yet exist
data NodeWithMaybeRef
  = NodeWithMaybeRef (Maybe NodeID) Node
  deriving (Show, Generic)

instance ToJSON NodeWithMaybeRef where
  toJSON (NodeWithMaybeRef ref node) =
    Aeson.object
      [ "id" .= ref,
        "kind" .= nodeKind node,
        "content" .= nodeContent node
      ]

-- a node version for a node which is guaranteed to already exist
data NodeWithRef
  = NodeWithRef NodeID Node
  deriving (Show, Generic)

instance ToJSON NodeWithRef where
  toJSON (NodeWithRef ref node) =
    Aeson.object
      [ "id" .= ref,
        "kind" .= nodeKind node,
        "content" .= nodeContent node
      ]

instance Hashable NodeWithRef where
  updateHash ctx (NodeWithRef ref node) =
    updateHash ctx ref
      & flip updateHash (nodeKind node)
      & flip updateHash (nodeContent node)

toNodeWithMaybeRef :: NodeWithRef -> NodeWithMaybeRef
toNodeWithMaybeRef (NodeWithRef ref node) = NodeWithMaybeRef (Just ref) node

-- a node version
data Node = Node
  { nodeKind    :: Text,
    nodeContent :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON Node

-- a relational-style edge
data DataEdge = DataEdge
  { edgeParent        :: Hash,
    edgeChild         :: Hash,
    edgeChildPosition :: Int32,
    edgeChildTitle    :: Text
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
