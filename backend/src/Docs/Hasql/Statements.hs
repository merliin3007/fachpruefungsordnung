{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Docs.Hasql.Statements
    ( createDocument
    , getDocument
    , createTextElement
    , getTextElement
    , createTextVersion
    , getTextVersion
    ) where

import Data.Profunctor (lmap, rmap)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Docs.Document (Document (Document), DocumentID (..))
import qualified Docs.Document as Document
import Docs.TextElement
    ( TextElement (TextElement)
    , TextElementID (..)
    )
import qualified Docs.TextElement as TextElement
import Docs.TextRevision
    ( TextRevision (TextRevision)
    , TextRevisionID (..)
    )
import qualified Docs.TextRevision as TextRevision
import GHC.Int (Int32)
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, singletonStatement)
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

createTextElement :: Statement Text TextElement
createTextElement =
    rmap
        uncurryTextElement
        [singletonStatement|
            insert into doc_text_elements
                (kind)
            values
                ($1 :: text)
            returning
                id :: int4,
                kind :: text
        |]

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

uncurryTextVersion :: (Int32, UTCTime, UUID, Text) -> TextRevision
uncurryTextVersion (id_, timestamp, author, content) =
    TextRevision
        { TextRevision.identifier = TextRevisionID id_
        , TextRevision.timestamp = timestamp
        , TextRevision.author = author
        , TextRevision.content = content
        }

createTextVersion :: Statement (TextElementID, UUID, Text) TextRevision
createTextVersion =
    lmap
        mapInput
        $ rmap
            uncurryTextVersion
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

getTextVersion :: Statement TextRevisionID (Maybe TextRevision)
getTextVersion =
    lmap
        unTextRevisionID
        $ rmap
            (uncurryTextVersion <$>)
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
