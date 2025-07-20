module Docs.Text
    ( TextElementID (..)
    , TextElement (..)
    , TextVersionID (..)
    , TextVersion (..)
    , TextElementVersion (..)
    , CreateTextElementVersion (..)
    ) where

import Data.Text (Text)
import Data.Time (LocalTime)
import Data.UUID (UUID)
import GHC.Int (Int32)

newtype TextElementID = TextElementID
    { unTextElementID :: Int32
    }

data TextElement = TextElement
    { textElementID :: TextElementID
    , textElementKind :: Text
    }

newtype TextVersionID = TextVersionID
    { unTextVersionID :: Int32
    }

data TextVersion = TextVersion
    { textVersionID :: TextVersionID
    , textVersionTimestamp :: LocalTime
    , textVersionAuthor :: UUID
    , textVersionContent :: Text
    }

data TextElementVersion
    = TextElementVersion
        TextElement
        TextVersion

data CreateTextElementVersion
    = CreateTextElementVersion
        TextElementID
        TextVersion
