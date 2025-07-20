module Docs.Client (Client (..)) where

import Data.Text (Text)

import Docs.Document (Document, DocumentID)
import UserManagement.Group (GroupID)

import Docs.TextRevision
    ( NewTextRevision
    , TextRevision
    , TextRevisionConflict
    )
import Docs.Util (UserID)

data Client m e = Client
    { createDocument :: Text -> GroupID -> m (Either e Document)
    , getDocument :: DocumentID -> m (Either e (Maybe Document))
    , createTextRevision
        :: UserID
        -> NewTextRevision
        -> m (Either e (Either TextRevisionConflict TextRevision))
    }
