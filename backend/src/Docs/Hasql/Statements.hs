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
    ) where

import Data.Profunctor (lmap, rmap)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Int (Int32)

import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, singletonStatement)

import Docs.Document (Document (Document), DocumentID (..))
import qualified Docs.Document as Document
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
    :: (Int32, TextElementKind, Int32, UTCTime, UUID, Text)
    -> TextElementRevision
uncurryTextElementRevision (id_, kind, revisionID, timestamp, author, content) =
    TextElementRevision
        TextElement
            { TextElement.identifier = TextElementID id_
            , TextElement.kind = kind
            }
        TextRevision
            { TextRevision.identifier = TextRevisionID revisionID
            , TextRevision.timestamp = timestamp
            , TextRevision.author = author
            , TextRevision.content = content
            }

createTextRevision :: Statement (TextElementID, UUID, Text) TextRevision
createTextRevision =
    lmap
        mapInput
        $ rmap
            uncurryTextRevision
            [singletonStatement|
                insert into doc_text_versions
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
                    doc_text_versions
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
                    doc_text_versions
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
                    tr.id :: int4,
                    tr.creation_ts :: timestamptz,
                    tr.author :: uuid,
                    tr.content :: text
                from
                    doc_text_versions tr
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
                    tr.id :: int4,
                    tr.creation_ts :: timestamptz,
                    tr.author :: uuid,
                    tr.content :: text
                from
                    doc_text_versions tr
                    join doc_text_elements te on te.id = tr.text_element
                where
                    te.id = $1 :: int4
                order by
                    tr.creation_ts desc
                limit 1
            |]
