{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
    , getTreeNodeMetadata
    , putTreeNode
    , putTreeEdge
    , putTreeRevision
    , getTextElementIDsForDocument
    ) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Profunctor (lmap, rmap)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
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
import Docs.Hasql.TreeEdge (TreeEdge, TreeEdgeChildRef (..))
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
import Docs.Tree (Node)
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

getTreeNodeMetadata :: Statement Hash Text
getTreeNodeMetadata =
    lmap
        unHash
        [singletonStatement|
            select
                metadata :: text
            from
                doc_tree_nodes
            where
                hash = $1 :: bytea
        |]

putTreeNode :: Statement (Hash, Text) ()
putTreeNode =
    lmap
        (first unHash)
        [resultlessStatement|
            insert into doc_tree_nodes
                (hash, metadata)
            values
                ($1 :: bytea, $2 :: text)
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
        (TreeEdgeToNode hash) -> Just $ unHash hash
        (TreeEdgeToTextElement _) -> Nothing
    childTextElement = case TreeEdge.child edge of
        (TreeEdgeToTextElement textElementID) -> Just $ unTextElementID textElementID
        (TreeEdgeToNode _) -> Nothing

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

uncurryTreeRevision :: (Int32, UTCTime, UserID) -> Node a -> TreeRevision a
uncurryTreeRevision (id_, timestamp, author) root =
    TreeRevision
        { TreeRevision.identifier = TreeRevisionID id_
        , TreeRevision.timestamp = timestamp
        , TreeRevision.author = author
        , TreeRevision.root = root
        }

putTreeRevision
    :: Statement (DocumentID, UserID, Hash) (Node a -> TreeRevision a)
putTreeRevision =
    lmap
        (\(docID, userID, rootHash) -> (unDocumentID docID, userID, unHash rootHash))
        $ rmap
            uncurryTreeRevision
            [singletonStatement|
                insert into doc_tree_revision
                    (document, author, root)
                values
                    ($1 :: int4, $2 :: uuid, $3 :: bytea)
                returning
                    id :: int4,
                    creation_ts :: timestamptz,
                    author :: uuid
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
