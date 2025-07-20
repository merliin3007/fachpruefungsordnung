module Docs.TextRevision
    ( TextRevisionID (..)
    , TextRevision (..)
    , TextElementRevision (..)
    , TextRevisionConflict (..)
    , NewTextRevision (..)
    , createTextRevision
    ) where

import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Docs.TextElement (TextElement, TextElementID)
import Docs.Util (UserID)
import GHC.Int (Int32)

newtype TextRevisionID = TextRevisionID
    { unTextRevisionID :: Int32
    }
    deriving (Eq)

data TextRevision = TextRevision
    { identifier :: TextRevisionID
    , timestamp :: UTCTime
    , author :: UUID
    , content :: Text
    }

data TextElementRevision
    = TextElementRevision
        TextElement
        TextRevision

data NewTextRevision = NewTextRevision
    { newTextRevisionAuthor :: UserID
    , newTextRevisionElement :: TextElementID
    , newTextRevisionParent :: Maybe TextRevisionID
    , newTextRevisionContent :: Text
    }

newtype TextRevisionConflict = TextRevisionConflict TextRevisionID -- todo: maybe not id but whole TextRevision?

-- | Creates a new text revision.
-- | Returns a conflict, if the parent revision is not the latest revision.
-- | If the text element does not have any revision, but a parent revision is set,
-- | it is ignored.
createTextRevision
    :: (Monad m)
    => (TextElementID -> m (Maybe TextRevisionID))
    -- ^ gets the latest revision id for a text element (if any)
    -> (TextElementID -> UserID -> Text -> m TextRevision)
    -- ^ creates a new text revision in the database
    -> NewTextRevision
    -- ^ all data needed to create a new text revision
    -> m (Either TextRevisionConflict TextRevision)
createTextRevision getLatestRevisionID createRevision newRevision = do
    latestRevisionID <- getLatestRevisionID $ newTextRevisionElement newRevision
    let parentRevisionID = newTextRevisionParent newRevision
    case latestRevisionID of
        Nothing -> createRevision' <&> Right
        Just latest
            | latestRevisionID == parentRevisionID -> createRevision' <&> Right
            | otherwise -> return $ Left $ TextRevisionConflict latest
  where
    createRevision' =
        createRevision
            (newTextRevisionElement newRevision)
            (newTextRevisionAuthor newRevision)
            (newTextRevisionContent newRevision)
