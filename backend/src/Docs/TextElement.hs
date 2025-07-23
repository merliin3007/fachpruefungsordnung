module Docs.TextElement
    ( TextElementID (..)
    , TextElement (..)
    , TextElementKind
    ) where

import DocumentManagement.Hash (Hashable (..))

import Data.Text (Text)
import GHC.Int (Int32)

newtype TextElementID = TextElementID
    { unTextElementID :: Int32
    }
    deriving (Eq, Ord)

instance Hashable TextElementID where
    updateHash ctx = updateHash ctx . unTextElementID

type TextElementKind = Text

data TextElement = TextElement
    { identifier :: TextElementID
    , kind :: TextElementKind
    }
