module FPO.Types where

import Prelude

import Ace.Types as Types
import Data.Array (find, sortBy)
import Data.DateTime (DateTime)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe)

type AnnotatedMarker =
  { id :: Int
  , type :: String
  , range :: Types.Range
  , startRow :: Int
  , startCol :: Int
  , mCommentSection :: Maybe CommentSection
  }

type CommentSection =
  { markerID :: Int
  , comments :: Array Comment
  , resolved :: Boolean
  }

type Comment =
  { author :: String
  , timestamp :: DateTime
  , content :: String
  }

type TOCEntry =
  { id :: Int
  , name :: String
  , content :: String
  , markers :: Array AnnotatedMarker
  }

-- shortend version for TOC component to not update its content
-- since it only uses id and name only
type ShortendTOCEntry =
  { id :: Int
  , name :: String
  }

findTOCEntry :: Int -> Array TOCEntry -> Maybe TOCEntry
findTOCEntry tocID tocEntries = find (\e -> e.id == tocID) tocEntries

findCommentSection :: Int -> Int -> Array TOCEntry -> Maybe CommentSection
findCommentSection tocID markerID tocEntries = do
  entry <- findTOCEntry tocID tocEntries
  marker <- find (\m -> m.id == markerID) entry.markers
  marker.mCommentSection

sortMarkers :: Array AnnotatedMarker -> Array AnnotatedMarker
sortMarkers = sortBy (comparing _.startRow <> comparing _.startCol)

markerToAnnotation :: AnnotatedMarker -> Types.Annotation
markerToAnnotation m =
  { row: m.startRow
  , column: m.startCol
  , text: "Comment found!"
  , type: m.type
  }

-- TODO create more timestamps versions and discuss, where to store this
timeStampsVersions :: Array Formatter
timeStampsVersions =
  [ ( DayOfMonthTwoDigits
        : Placeholder " "
        : MonthShort
        : Placeholder " "
        : YearTwoDigits
        : Placeholder " "
        : Hours24
        : Placeholder ":"
        : MinutesTwoDigits
        : Nil
    )
  ]