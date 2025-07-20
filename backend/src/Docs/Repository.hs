module Docs.Repository
    ( Repository (..)
    ) where

import Data.Text (Text)
import Docs.TextElement (TextElement, TextElementID)
import Docs.TextRevision (TextRevision, TextRevisionID)
import Docs.Util (UserID)

data (Monad m) => Repository m = Repository
    { getLatestTextRevisionID :: TextElementID -> m (Maybe TextRevisionID)
    , createTextRevision :: TextElementID -> UserID -> Text -> m TextRevision
    , getTextElement :: TextElementID -> m TextElement
    }
