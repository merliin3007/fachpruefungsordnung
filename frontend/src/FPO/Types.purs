module FPO.Types where

import Prelude

import Ace.Types as Types
import Data.Array (find, mapWithIndex, sortBy)
import Data.DateTime (DateTime)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import FPO.Dto.DocumentDto.DocumentTree as DT
import FPO.Dto.DocumentDto.NodeHeader as NH
import FPO.Dto.DocumentDto.TreeDto (Edge(..), RootTree(..), Tree(..), findRootTree, findTitleRootTree, replaceNodeRootTree)

-- TODO We can also store different markers, such as errors. But do we want to?
type AnnotatedMarker =
  { id :: Int
  , type :: String
  , startRow :: Int
  , startCol :: Int
  , endRow :: Int
  , endCol :: Int
  , markerText :: String
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
  -- paragraph ID (§id) Automatically generated with enumTOCTree
  , paraID :: Int
  -- Is stored as 32bit Int = 2,147,483,647
  -- Schould not create so many markers, right?
  , newMarkerNextID :: Int
  , markers :: Array AnnotatedMarker
  }

-- shortend version for TOC component to not update its content
-- since it only uses id and name only
type ShortendTOCEntry =
  { id :: Int
  , paraID :: Int
  , name :: String
  }

type TOCTree = RootTree TOCEntry

-- Empty TOCEntry in case of errors
emptyTOCEntry :: TOCEntry
emptyTOCEntry =
  { id: -1
  , name: "Error"
  , paraID: -1
  , newMarkerNextID: -1
  , markers: []
  }

findTOCEntry :: Int -> TOCTree -> Maybe TOCEntry
findTOCEntry tocID = findRootTree (\e -> e.id == tocID)

findTitleTOCEntry :: Int -> TOCTree -> Maybe String
findTitleTOCEntry tocID = findTitleRootTree (\e -> e.id == tocID)

replaceTOCEntry :: Int -> String -> TOCEntry -> TOCTree -> TOCTree
replaceTOCEntry tocID = replaceNodeRootTree (\e -> e.id == tocID)

findCommentSection :: Int -> Int -> TOCTree -> Maybe CommentSection
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
  , text: m.markerText
  , type: m.type
  }

shortenTOC :: TOCEntry -> ShortendTOCEntry
shortenTOC { id, paraID, name } = { id, paraID, name }

-- TODO create more timestamps versions and discuss, where to store this
timeStampsVersions :: Array Formatter
timeStampsVersions =
  [ -- DD.MM.YY HH:mm
    ( DayOfMonthTwoDigits
        : Placeholder "."
        : MonthShort
        : Placeholder "."
        : YearTwoDigits
        : Placeholder " "
        : Hours24
        : Placeholder ":"
        : MinutesTwoDigits
        : Nil
    )
  -- DD/MM/YY HH:mm
  , ( DayOfMonthTwoDigits
        : Placeholder "/"
        : MonthShort
        : Placeholder "/"
        : YearTwoDigits
        : Placeholder " "
        : Hours24
        : Placeholder ":"
        : MinutesTwoDigits
        : Nil
    )
  -- MM/DD/YY HH:mm
  , ( MonthShort
        : Placeholder "/"
        : DayOfMonthTwoDigits
        : Placeholder "/"
        : YearTwoDigits
        : Placeholder " "
        : Hours24
        : Placeholder ":"
        : MinutesTwoDigits
        : Nil
    )
  -- YY/MM/DD HH:mm
  , ( YearTwoDigits
        : Placeholder "/"
        : MonthShort
        : Placeholder "/"
        : DayOfMonthTwoDigits
        : Placeholder " "
        : Hours24
        : Placeholder ":"
        : MinutesTwoDigits
        : Nil
    )
  ]

-- Tree functions for TOC

nodeHeaderToTOCEntry :: NH.NodeHeader -> TOCEntry
nodeHeaderToTOCEntry nh =
  { id: NH.getId nh
  , name: NH.getKind nh
  , paraID: 0 -- later automatically generated with enumerateTOCTree
  , newMarkerNextID: 0
  , markers: []
  }

enumTOCTree :: TOCTree -> TOCTree
enumTOCTree Empty = Empty
enumTOCTree (RootTree { children, header }) =
  let
    newChildren = mapWithIndex (\i (Edge child) -> Edge $ enumTOCTree' (i+1) child) children
  in
    RootTree { children: newChildren, header }

enumTOCTree' :: Int -> Tree TOCEntry -> Tree TOCEntry
enumTOCTree' id (Leaf { title, node }) =
  let
    newNode = node { paraID= id }
  in
  Leaf { title, node: newNode }
enumTOCTree' _ (Node { title, children, header }) =
  let
    newChildren = mapWithIndex (\i (Edge child) -> Edge $ enumTOCTree' (i+1) child) children
  in
    Node { title, children: newChildren, header }

tocEntryToNodeHeader :: TOCEntry -> NH.NodeHeader
tocEntryToNodeHeader { id, name } =
  NH.NodeHeader { identifier: id, kind: name }

documentTreeToTOCTree :: DT.DocumentTree -> TOCTree
documentTreeToTOCTree dtTree = 
  let 
    tmpTree = map nodeHeaderToTOCEntry dtTree
  in
    enumTOCTree tmpTree

tocTreeToDocumentTree :: TOCTree -> DT.DocumentTree
tocTreeToDocumentTree = map tocEntryToNodeHeader
