module Docs.Hasql.TreeEdge
    ( TreeEdge (..)
    , TreeEdgeChildRef (..)
    ) where

import Data.Text (Text)
import GHC.Int (Int32)

import Docs.TextElement (TextElementID)
import DocumentManagement.Hash (Hash, Hashable (..))

data TreeEdgeChildRef
    = TreeEdgeToTextElement TextElementID
    | TreeEdgeToNode Hash

instance Hashable TreeEdgeChildRef where
    updateHash ctx (TreeEdgeToTextElement elementID) =
        updateHash ctx elementID
    updateHash ctx (TreeEdgeToNode nodeHash) = updateHash ctx nodeHash

data TreeEdge = TreeEdge
    { parentHash :: Hash
    , position :: Int32
    , title :: Text
    , child :: TreeEdgeChildRef
    }
