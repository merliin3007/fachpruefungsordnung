module Docs.TreeRevision
    ( TreeRevisionID (..)
    , TreeRevision (..)
    , TreeRevisionSelector (..)
    , mapRoot
    , mapMRoot
    , replaceRoot
    , withTextRevisions
    , newTreeRevision
    ) where

import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Int (Int32)

import Control.Monad (unless)
import Docs.Document (DocumentID)
import Docs.TextElement (TextElement, TextElementID)
import Docs.TextRevision (TextElementRevision, TextRevision)
import Docs.Tree (Node)
import qualified Docs.Tree as Tree
import Docs.Util (UserID)

data TreeRevisionSelector
    = Latest DocumentID
    | Specific TreeRevisionID

newtype TreeRevisionID = TreeRevisionID
    { unTreeRevisionID :: Int32
    }
    deriving (Eq)

data TreeRevision a = TreeRevision
    { identifier :: TreeRevisionID
    , timestamp :: UTCTime
    , author :: UUID
    , root :: Node a
    }

mapRoot :: (Node a -> Node b) -> TreeRevision a -> TreeRevision b
mapRoot f treeRevision =
    TreeRevision
        { identifier = identifier treeRevision
        , timestamp = timestamp treeRevision
        , author = author treeRevision
        , root = f $ root treeRevision
        }

replaceRoot :: Node b -> TreeRevision a -> TreeRevision b
replaceRoot new = mapRoot $ const new

mapMRoot
    :: (Monad m)
    => (Node a -> m (Node b))
    -> TreeRevision a
    -> m (TreeRevision b)
mapMRoot f treeVersion = do
    new <- f $ root treeVersion
    return $ replaceRoot new treeVersion

instance Functor TreeRevision where
    fmap f treeVersion =
        TreeRevision
            { identifier = identifier treeVersion
            , timestamp = timestamp treeVersion
            , author = author treeVersion
            , root = f <$> root treeVersion
            }

-- | Takes a tree revision and emplaces concrecte text revisions.
-- | The text revisions are obtained via the specified getter function.
withTextRevisions
    :: (Monad m)
    => (TextElementID -> m (Maybe TextRevision))
    -- ^ (potentially effectful) function for obtaining a text revision
    -> TreeRevision TextElement
    -- ^ document structre tree revision
    -> m (TreeRevision TextElementRevision)
    -- ^ document structre tree revision with concrete text revision
withTextRevisions getTextRevision = mapMRoot (Tree.withTextRevisions getTextRevision)

newTreeRevision
    :: (Monad m)
    => (DocumentID -> m (TextElementID -> Bool))
    -- ^ for a given document, checks if a text element belongs to this document
    -> (UserID -> DocumentID -> Node TextElementID -> m (TreeRevision TextElementID))
    -- ^ create a new tree revision
    -> UserID
    -- ^ authors user id
    -> DocumentID
    -- ^ which document the revision belongs to
    -> Node TextElementID
    -- ^ the root node of the revision
    -> m (TreeRevision TextElementID)
    -- ^ newly created revision
newTreeRevision
    isTextElementInDocument
    createRevision
    authorID
    docID
    rootNode = do
        isTextElementInDocument' <- isTextElementInDocument docID
        let allTextElementsBelongToDocument =
                all isTextElementInDocument' rootNode
        unless allTextElementsBelongToDocument $
            error "Not all referenced text elements belong to the document."
        createRevision authorID docID rootNode
