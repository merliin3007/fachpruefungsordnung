module Docs.TextElement
    ( TextElementID (..)
    , TextElement (..)
    ) where

import Data.Text (Text)
import GHC.Int (Int32)

newtype TextElementID = TextElementID
    { unTextElementID :: Int32
    }

data TextElement = TextElement
    { identifier :: TextElementID
    , kind :: Text
    }
