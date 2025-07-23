{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Docs.Hasql.Statements
    ( createDocument
    , getDocument
    , createTextElement
    , getTextElement
    , createTextRevision
    , getTextRevision
    , getLatestTextRevisionID
    , getTextElementRevision
    , getLatestTextElementRevision
    , getTreeNode
    , putTreeNode
    , putTreeEdge
    , putTreeRevision
    , getTreeRevision
    , getLatestTreeRevision
    , getTextElementIDsForDocument
    , getTreeEdgesByParent
    ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Profunctor (lmap, rmap)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Tuple.Curry (uncurryN)
import Data.UUID (UUID)
import Data.Vector (Vector, mapMaybe)
import GHC.Int (Int32)

import Hasql.Statement (Statement)
import Hasql.TH
    ( maybeStatement
    , resultlessStatement
    , singletonStatement
    , vectorStatement
    )

import Docs.Document (Document (Document), DocumentID (..))
import qualified Docs.Document as Document
import Docs.Hasql.TreeEdge
    ( TreeEdge
    , TreeEdgeChild (..)
    , TreeEdgeChildRef (..)
    )
import qualified Docs.Hasql.TreeEdge as TreeEdge
import Docs.TextElement
    ( TextElement (TextElement)
    , TextElementID (..)
    , TextElementKind
    )
import qualified Docs.TextElement as TextElement
import Docs.TextRevision
    ( TextElementRevision (TextElementRevision)
    , TextRevision (TextRevision)
    , TextRevisionID (..)
    )
import qualified Docs.TextRevision as TextRevision
import Docs.Tree (Node, NodeHeader (NodeHeader))
import qualified Docs.Tree as Tree
import Docs.TreeRevision (TreeRevision (TreeRevision), TreeRevisionID (..))
import qualified Docs.TreeRevision as TreeRevision
import Docs.Util (UserID)
import DocumentManagement.Hash (Hash (..))
import UserManagement.Group (GroupID)

uncurryDocument :: (Int32, Text, Int32) -> Document
uncurryDocument (id_, name, groupID) =
    Document
        { Document.identifier = DocumentID id_
        , Document.name = name
        , Document.group = groupID
        }

createDocument :: Statement (Text, GroupID) Document
createDocument =
    rmap
        uncurryDocument
        [singletonStatement|
            insert into docs
                (name, "group")
            values
                ($1 :: text, $2 :: int4)
            returning
                id :: int4,
                name :: text,
                group_id :: int4
        |]

getDocument :: Statement DocumentID (Maybe Document)
getDocument =
    lmap unDocumentID $
        rmap
            (uncurryDocument <$>)
            [maybeStatement|
                select
                    id :: int4,
                    name :: text,
                    "group" :: int4
                from
                    docs
                where
                    id = $1 :: int4
            |]

uncurryTextElement :: (Int32, Text) -> TextElement
uncurryTextElement (id_, kind) =
    TextElement
        { TextElement.identifier = TextElementID id_
        , TextElement.kind = kind
        }

createTextElement :: Statement (DocumentID, Text) TextElement
createTextElement =
    lmap mapInput $
        rmap
            uncurryTextElement
            [singletonStatement|
            insert into doc_text_elements
                (document, kind)
            values
                ($1 :: int4, $2 :: text)
            returning
                id :: int4,
                kind :: text
        |]
  where
    mapInput (documentID, kind) = (unDocumentID documentID, kind)

getTextElement :: Statement TextElementID (Maybe TextElement)
getTextElement =
    lmap
        unTextElementID
        $ rmap
            (uncurryTextElement <$>)
            [maybeStatement|
                select
                    id :: int4,
                    kind :: text
                from
                    doc_text_elements
                where
                    id = $1 :: int4
            |]

uncurryTextRevision :: (Int32, UTCTime, UUID, Text) -> TextRevision
uncurryTextRevision (id_, timestamp, author, content) =
    TextRevision
        { TextRevision.identifier = TextRevisionID id_
        , TextRevision.timestamp = timestamp
        , TextRevision.author = author
        , TextRevision.content = content
        }

uncurryTextElementRevision
    :: (Int32, TextElementKind, Maybe Int32, Maybe UTCTime, Maybe UUID, Maybe Text)
    -> TextElementRevision
uncurryTextElementRevision (id_, kind, revisionID, timestamp, author, content) =
    TextElementRevision
        TextElement
            { TextElement.identifier = TextElementID id_
            , TextElement.kind = kind
            }
        $ do
            trRevisionID <- revisionID
            trTimestamp <- timestamp
            trAuthor <- author
            trContent <- content
            return $
                TextRevision
                    { TextRevision.identifier = TextRevisionID trRevisionID
                    , TextRevision.timestamp = trTimestamp
                    , TextRevision.author = trAuthor
                    , TextRevision.content = trContent
                    }

createTextRevision :: Statement (TextElementID, UUID, Text) TextRevision
createTextRevision =
    lmap
        mapInput
        $ rmap
            uncurryTextRevision
            [singletonStatement|
                insert into doc_text_revisions
                    (text_element, author, content)
                values
                    ($1 :: int4, $2 :: uuid, $3 :: text)
                returning
                    id :: int4,
                    creation_ts :: timestamptz,
                    author :: uuid,
                    content :: text
            |]
  where
    mapInput (elementID, author, content) =
        (unTextElementID elementID, author, content)

getTextRevision :: Statement TextRevisionID (Maybe TextRevision)
getTextRevision =
    lmap
        unTextRevisionID
        $ rmap
            (uncurryTextRevision <$>)
            [maybeStatement|
                select
                    id :: int4,
                    creation_ts :: timestamptz,
                    author :: uuid,
                    content :: text
                from
                    doc_text_revisions
                where
                    id = $1 :: int4
            |]

getLatestTextRevisionID :: Statement TextElementID (Maybe TextRevisionID)
getLatestTextRevisionID =
    lmap
        unTextElementID
        $ rmap
            (TextRevisionID <$>)
            [maybeStatement|
                select
                    id :: int4
                from
                    doc_text_revisions
                where
                    text_element = $1 :: int4
                order by
                    creation_ts desc
                limit 1
            |]

getTextElementRevision :: Statement TextRevisionID (Maybe TextElementRevision)
getTextElementRevision =
    lmap
        unTextRevisionID
        $ rmap
            (uncurryTextElementRevision <$>)
            [maybeStatement|
                select
                    te.id :: int4,
                    te.kind :: text,
                    tr.id :: int4?,
                    tr.creation_ts :: timestamptz?,
                    tr.author :: uuid?,
                    tr.content :: text?
                from
                    doc_text_revisions tr
                    join doc_text_elements te on te.id = tr.text_element
                where
                    tr.id = $1 :: int4
            |]

getLatestTextElementRevision
    :: Statement TextElementID (Maybe TextElementRevision)
getLatestTextElementRevision =
    lmap
        unTextElementID
        $ rmap
            (uncurryTextElementRevision <$>)
            [maybeStatement|
                select
                    te.id :: int4,
                    te.kind :: text,
                    tr.id :: int4?,
                    tr.creation_ts :: timestamptz?,
                    tr.author :: uuid?,
                    tr.content :: text?
                from
                    doc_text_revisions tr
                    join doc_text_elements te on te.id = tr.text_element
                where
                    te.id = $1 :: int4
                order by
                    tr.creation_ts desc
                limit 1
            |]

getTreeNode :: Statement Hash NodeHeader
getTreeNode =
    lmap
        unHash
        $ rmap
            (uncurryN NodeHeader)
            [singletonStatement|
            select
                kind :: text,
                type :: text
            from
                doc_tree_nodes
            where
                hash = $1 :: bytea
        |]

putTreeNode :: Statement (Hash, NodeHeader) ()
putTreeNode =
    lmap
        ( \(hash, header) ->
            ( unHash hash
            , Tree.headerKind header
            , Tree.headerType header
            )
        )
        [resultlessStatement|
            insert into doc_tree_nodes
                (hash, kind, type)
            values
                ($1 :: bytea, $2 :: text, $3 :: text)
            on conflict do nothing
        |]

uncurryTreeEdge
    :: TreeEdge
    -> (ByteString, Int32, Text, Maybe ByteString, Maybe Int32)
uncurryTreeEdge edge =
    ( unHash (TreeEdge.parentHash edge)
    , TreeEdge.position edge
    , TreeEdge.title edge
    , childNode
    , childTextElement
    )
  where
    childNode = case TreeEdge.child edge of
        (TreeEdgeRefToNode hash) -> Just $ unHash hash
        (TreeEdgeRefToTextElement _) -> Nothing
    childTextElement = case TreeEdge.child edge of
        (TreeEdgeRefToTextElement textElementID) -> Just $ unTextElementID textElementID
        (TreeEdgeRefToNode _) -> Nothing

putTreeEdge :: Statement TreeEdge ()
putTreeEdge =
    lmap
        uncurryTreeEdge
        [resultlessStatement|
            insert into doc_tree_edges
                ( parent
                , position
                , title
                , child_node
                , child_text_element
                )
            values
                ( $1 :: bytea
                , $2 :: int4
                , $3 :: text
                , $4 :: bytea?
                , $5 :: int4?
                )
            on conflict do nothing
        |]

uncurryTreeEdgeChild
    :: ( Text
       , Maybe ByteString
       , Maybe Text
       , Maybe Text
       , Maybe Int32
       , Maybe Text
       )
    -> Maybe (Text, TreeEdgeChild)
uncurryTreeEdgeChild (title, nodeHash, nodeKind, nodeType, textID, textKind) =
    (title,) <$> (maybeNode <|> maybeText)
  where
    maybeNode = do
        hash <- nodeHash
        kind <- nodeKind
        type_ <- nodeType
        return $
            TreeEdgeToNode
                (Hash hash)
                NodeHeader
                    { Tree.headerKind = kind
                    , Tree.headerType = type_
                    }
    maybeText = do
        id_ <- textID
        kind <- textKind
        return $
            TreeEdgeToTextElement
                TextElement
                    { TextElement.identifier = TextElementID id_
                    , TextElement.kind = kind
                    }

getTreeEdgesByParent :: Statement Hash (Vector (Text, TreeEdgeChild))
getTreeEdgesByParent =
    lmap
        unHash
        $ rmap
            (mapMaybe uncurryTreeEdgeChild)
            [vectorStatement|
                select
                    e.title :: text,
                    n.hash :: bytea?,
                    n.kind :: text?,
                    n.type :: text?,
                    t.id :: int4?,
                    t.kind :: text?
                from
                    doc_tree_edges e
                    join doc_tree_nodes n on e.child_node = n.id
                    join doc_text_elements t on e.child_text_element = t.id
                where
                    e.parent = $1 :: bytea
            |]

uncurryTreeRevision :: (Int32, UTCTime, UserID) -> Node a -> TreeRevision a
uncurryTreeRevision (id_, timestamp, author) root =
    TreeRevision
        { TreeRevision.identifier = TreeRevisionID id_
        , TreeRevision.timestamp = timestamp
        , TreeRevision.author = author
        , TreeRevision.root = root
        }

uncurryTreeRevisionWithRoot
    :: (Int32, UTCTime, UserID, ByteString)
    -> (Hash, Node a -> TreeRevision a)
uncurryTreeRevisionWithRoot (id_, ts, author, root) =
    (Hash root, uncurryTreeRevision (id_, ts, author))

putTreeRevision
    :: Statement (DocumentID, UserID, Hash) (Node a -> TreeRevision a)
putTreeRevision =
    lmap
        mapInput
        $ rmap
            uncurryTreeRevision
            [singletonStatement|
                insert into doc_tree_revisions
                    (document, author, root)
                values
                    ($1 :: int4, $2 :: uuid, $3 :: bytea)
                returning
                    id :: int4,
                    creation_ts :: timestamptz,
                    author :: uuid
            |]
  where
    mapInput (docID, userID, rootHash) =
        (unDocumentID docID, userID, unHash rootHash)

getTreeRevision :: Statement TreeRevisionID (Hash, Node a -> TreeRevision a)
getTreeRevision =
    lmap
        unTreeRevisionID
        $ rmap
            uncurryTreeRevisionWithRoot
            [singletonStatement|
                select
                    id :: int4,
                    creation_ts :: timestamptz,
                    author :: uuid,
                    root :: bytea
                from
                    doc_tree_revisions
                where
                    id = $1 :: int4
            |]

getLatestTreeRevision :: Statement DocumentID (Hash, Node a -> TreeRevision a)
getLatestTreeRevision =
    lmap
        unDocumentID
        $ rmap
            uncurryTreeRevisionWithRoot
            [singletonStatement|
                select
                    id :: int4,
                    creation_ts :: timestamptz,
                    author :: uuid,
                    root :: bytea
                from
                    doc_tree_revisions
                where
                    document = $1 :: int4
                order by
                    creation_ts desc
                limit 1
            |]

getTextElementIDsForDocument :: Statement DocumentID (Vector TextElementID)
getTextElementIDsForDocument =
    lmap unDocumentID $
        rmap
            (TextElementID <$>)
            [vectorStatement|
                select
                    id :: int4
                from
                    doc_text_elements
                where
                    document = $1 :: int4
            |]
