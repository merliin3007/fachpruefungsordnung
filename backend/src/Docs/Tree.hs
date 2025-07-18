module Docs.Tree
    ( NodeID (..)
    , NodeHeader (..)
    , Tree (..)
    , Edge (..)
    , TreeVersionID (..)
    , TreeVersion (..)
    ) where

import Data.Text (Text)
import Data.Time (LocalTime)
import Data.UUID (UUID)
import DocumentManagement.Hash (Hashed)
import GHC.Int (Int32)

newtype NodeID = NodeID
    { unNodeID :: Int32
    }

data NodeHeader = NodeHeader
    { nodeHeaderID :: NodeID
    , nodeHeaderMetaData :: Hashed Text
    }

data Tree a = Node NodeHeader [Edge a] | Leaf a

data Edge a = Edge Text (Tree a)

newtype TreeVersionID = StructureVersionID
    { unTreeVersionID :: Int32
    }

data TreeVersion a = TreeVersion
    { treeVersionID :: TreeVersionID
    , treeVersionTimestamp :: LocalTime
    , treeVersionAuthor :: UUID
    , treeVersionRoot :: Tree a
    }
