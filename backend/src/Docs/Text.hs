module Docs.Text
    ( TextElementID (..)
    , TextElement (..)
    , TextVersionID (..)
    , TextVersion (..)
    , ExistingTextVersion (..)
    , CreateTextVersion (..)
    ) where

import Data.Text (Text)
import Data.Time (LocalTime)
import Data.UUID (UUID)
import DocumentManagement.Hash (Hashed)
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
    { textVersionTimestamp :: LocalTime
    , textVersionAuthor :: UUID
    , textVersionContent :: Hashed Text
    }

data ExistingTextVersion
    = ExistingTextVersion
        TextVersionID
        TextElement
        TextVersion

data CreateTextVersion
    = CreateTextVersion
        TextElementID
        TextVersion
