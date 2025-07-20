module Docs
    ( Context (..)
    , getDocument
    , getTreeVersion
    , createTreeVersion
    , createTextRevision
    )
where

import Docs.Document (Document, DocumentID)
import Docs.Repository (Repository)
import qualified Docs.Repository as Repository
import Docs.TextElement (TextElement)
import Docs.TextRevision
    ( NewTextRevision
    , TextRevision
    , TextRevisionConflict
    , newTextRevision
    )
import Docs.Tree (Tree)
import Docs.TreeRevision (ExistingTreeRevision, TreeRevisionID)
import Docs.Util (UserID)

data (Monad m) => Context m = Context
    { contextRepository :: Repository m
    , contextUser :: UserID
    }

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

-- | Create a new revision for a text element.
createTextRevision
    :: (Monad m)
    => Context m
    -- ^ document management context
    -> NewTextRevision
    -- ^ new text revision
    -> m (Either TextRevisionConflict TextRevision)
    -- ^ either a new text revision or a conflict
createTextRevision ctx =
    newTextRevision
        (Repository.getLatestTextRevisionID repo)
        (Repository.createTextRevision repo)
        user
  where
    repo = contextRepository ctx
    user = contextUser ctx
