module Docs.Client (Client (..)) where

import Data.Text (Text)

import Docs.Document (Document, DocumentID)
import UserManagement.Group (GroupID)

import Docs.TextElement (TextElement, TextElementKind)
import Docs.TextRevision
    ( NewTextRevision
    , TextElementRevision
    , TextRevision
    , TextRevisionConflict
    , TextRevisionSelector
    )
import Docs.Util (UserID)

-- | A document management client.
-- | Note: This client does not check for access rights as it is the responsibility of
-- | the user management module.
data Client m e = Client
    { createDocument :: Text -> GroupID -> m (Either e Document)
    , getDocument :: DocumentID -> m (Either e (Maybe Document))
    , createTextElement :: DocumentID -> TextElementKind -> m (Either e TextElement)
    , createTextRevision
        :: UserID
        -> NewTextRevision
        -> m (Either e (Either TextRevisionConflict TextRevision))
    , getTextElementRevision
        :: TextRevisionSelector
        -> m (Either e (Maybe TextElementRevision))
    }
