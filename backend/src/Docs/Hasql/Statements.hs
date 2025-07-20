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
import Docs.Document (Document (..), DocumentID (..))
import Docs.Text
    ( TextElement (..)
    , TextElementID (..)
    , TextVersion (..)
    , TextVersionID (..)
    )
import GHC.Int (Int32)
import Hasql.Statement (Statement)
import Hasql.TH
import UserManagement.Group (GroupID)

uncurryDocument :: (Int32, Text, Int32) -> Document
uncurryDocument (id_, name, groupID) =
    Document
        (DocumentID id_)
        name
        groupID

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
        (TextElementID id_)
        kind

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

uncurryTextVersion :: (Int32, UTCTime, UUID, Text) -> TextVersion
uncurryTextVersion (id_, timestamp, author, content) =
    TextVersion
        { textVersionID = TextVersionID id_
        , textVersionTimestamp = timestamp
        , textVersionAuthor = author
        , textVersionContent = content
        }

createTextVersion :: Statement (TextElementID, UUID, Text) TextVersion
createTextVersion =
    lmap
        (\(elementID, author, content) -> (unTextElementID elementID, author, content))
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

getTextVersion :: Statement TextVersionID (Maybe TextVersion)
getTextVersion =
    lmap
        unTextVersionID
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
