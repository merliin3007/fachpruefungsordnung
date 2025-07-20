module Docs
    ( DocsRepository (..)
    , Context (..)
    , getDocument
    , getTreeVersion
    , createTreeVersion
    )
where

import Data.UUID (UUID)
import Docs.Document (Document, DocumentID)
import Docs.TextElement (TextElement, TextElementID)
import Docs.TextRevision (TextElementRevision)
import Docs.Tree (Tree)
import Docs.TreeRevision (ExistingTreeRevision, TreeRevisionID)

data (Monad m) => DocsRepository m = DocsRepository
    { getLatestTextElementVersion :: TextElementID -> m TextElementRevision
    , getTextElement :: TextElementID -> m TextElement
    }

data (Monad m) => Context m = Context (DocsRepository m) UUID

getDocument
    :: (Monad m)
    => Context m
    -- ^ document management context
    -> DocumentID
    -- ^ id of the document to obtain
    -> m (Maybe Document)
    -- ^ the corresponding document (if exists)
getDocument = undefined

getTreeVersion
    :: (Monad m)
    => Context m
    -- ^ document management context
    -> TreeRevisionID
    -- ^ the id of the concrete version to obtain
    -> m (Maybe (ExistingTreeRevision TextElement))
    -- ^ the corresponding tree version (if exists)
getTreeVersion = undefined

createTreeVersion
    :: (Monad m)
    => Context m
    -- ^ document management context
    -> TreeRevisionID
    -- ^ id of the parent version
    -> Tree TextElement
    -- ^ the new tree
    -> m (ExistingTreeRevision TextElement)
    -- ^ the newly created tree version
createTreeVersion = undefined
