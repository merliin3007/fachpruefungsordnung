module Docs.TextRevision
    ( TextRevisionID (..)
    , TextRevision (..)
    , TextElementRevision (..)
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Docs.TextElement (TextElement)
import GHC.Int (Int32)

newtype TextRevisionID = TextRevisionID
    { unTextRevisionID :: Int32
    }

data TextRevision = TextRevision
    { identifier :: TextRevisionID
    , timestamp :: UTCTime
    , author :: UUID
    , content :: Text
    }

data TextElementRevision
    = TextElementRevision
        TextElement
        TextRevision
