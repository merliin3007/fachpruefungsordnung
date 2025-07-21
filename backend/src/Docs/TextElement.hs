module Docs.TextElement
    ( TextElementID (..)
    , TextElement (..)
    , TextElementKind
    ) where

import Data.Text (Text)
import GHC.Int (Int32)

newtype TextElementID = TextElementID
    { unTextElementID :: Int32
    }
    deriving (Eq)

type TextElementKind = Text

data TextElement = TextElement
    { identifier :: TextElementID
    , kind :: TextElementKind
    }
