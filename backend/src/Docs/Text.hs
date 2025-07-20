module Docs.Text
    ( TextElementID (..)
    , TextElement (..)
    , TextVersionID (..)
    , TextVersion (..)
    , TextElementVersion (..)
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)
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
    , textVersionTimestamp :: UTCTime
    , textVersionAuthor :: UUID
    , textVersionContent :: Text
    }

data TextElementVersion
    = TextElementVersion
        TextElement
        TextVersion
