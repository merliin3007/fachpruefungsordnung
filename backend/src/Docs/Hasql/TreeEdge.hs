module Docs.Hasql.TreeEdge (TreeEdge (..)) where

import Data.Text (Text)
import GHC.Int (Int32)

import Docs.TextElement (TextElementID)
import DocumentManagement.Hash (Hash)

data TreeEdge = TreeEdge
    { parentHash :: Hash
    , position :: Int32
    , title :: Text
    , child :: Either TextElementID Hash
    }
